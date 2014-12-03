# respond
a Haskell library built on top of WAI for processing and routing HTTP requests
and generating responses.

## using respond
you'll probably want to look at the haddock documentation (link to come after 
release) while you're reading this guide.
let's get started with a brief overview of how to use this library.

### a brief overview
building an app using respond should hopefully be straightforward, and familiar 
to anyone who has used other WAI-based web service libraries (e.g. scotty).
* first, you integrate the `RespondT` monad transformer into your monad stack. you 
  can also use `RespondM` if you don't need a more involved stack.
* use various routing tools (defined in terms of `MonadRespond`) to match various 
  aspects of the request
* produce responses from inside your routing, either by building them directly, 
  or by using one of the tools for building responses
* convert your routing stack into a WAI app and run it in a WAI compatible 
  server; this library provides a default warp setup you can use.

### the monad transformer and monadic interface
MonadRespond is the monadic interface that most of the tools this library 
provides build on top of. it defines a core set of actions:
* `respond` is the WAI 3.0 `Application` continuation lifted into `MonadRespond`. 
  whenever you see the type `ResponseReceived`, a call to respond is involved.
* `getRequest` gets the request that's currently being handled. if you ever 
  find yourself using this directly, get in touch; there's probably an 
  opportunity to add a new tool to the library or improve the existing ones!
* `getREHs` gets out the `RequestErrorHandlers`. these define the responding
  action to use when request matching or processing fails.
* `withREHs` runs the inner `MonadRespond` action with a modified set of
  handlers; you can use this to add code to be run after the actual response is
  sent, or to change the response entirely.
* `getPath` gets the current `PathConsumer`.
* `withPath` runs the passed `MonadRespond` action with a modified `PathConsumer`
  value. this and the previous function will be explained further in the
  discussion of path routing.

the WAI `Application` type specifies that both the passed continuation and the
returned value are in `IO`; because MonadRespond specifies a wrapping of that
continuation, any instance of `MonadRespond` must be an instance of `MonadIO`.

`RespondT` is the monad transformer that implements the `MonadRespond` class (as
long as it is stacked on top of a `MonadIO`); it's basically a newtype for a
`ReaderT` that contains a record type containing the relevant components. there is
a simple `RespondM` type alias defined in the `Web.Respond.Run` module; this
alias stacks `RespondT` directly on `IO`, i.e.

```haskell
type RespondM a = RespondT IO a 
```


### running the app
there are several functions in `Web.Respond.Run` that you can use. the key ones
build a WAI application from a `RespondT` route (i.e. a value of type `RespondT m
ResponseReceived`); note that these functions (`respondApp` and `respondAppDefault`)
require a function to run the rest of the monadic stack to an `IO` value.
(the `RespondM` functions, of course, do not require this run function - running
the `RespondT` already produces the necessary `IO` value.)

the default handlers used by the \*Default functions are contained in
`Web.Respond.DefaultHandlers`; the default warp server setup used by the serve\*
functions are contained in `Web.Respond.DefaultServer`.

### request processing and routing
a number of tools are provided for (hopefully) convenient request processing and
routing.

#### matching methods
`Web.Respond.Method` defines the type `MethodMatcher`, which is just a newtype
around a Map from `StandardMethod` to some value. the module provides an
`onMethod` function that takes a `StandardMethod` and a value and provide a
`MethodMatcher` for just that method. functions are provided that apply
`onMethod` to each `StandardMethod`. using the monoid instance of
`MethodMatcher`, you can combine matchers to map different methods to different
actions. for example:

```haskell
-- | do something
act1 :: MonadRespond m => m ResponseReceived
act1 = ...

-- | do something else
act2 :: MonadRespond m => m ResponseReceived
act2 = ...

getOrPutActions ::  MonadRespond m => MethodMatcher (m ResponseReceived)
getOrPutActions = onGET act1 <> onPOST act2
```

`matchMethod` takes a `MethodMatcher` that will produce an appropriate responding
action for each mapped HTTP method, and chooses the action that handles the
current request's method. 

```haskell
-- continuing from before ...
route :: MonadRespond m => m ResponseReceived
route = matchMethod $ 
    onGET act1 <>
    onPOST act2
```

in this example, route will use act1 if the request method is GET, act2 if the
method is POST, and will call `handleUnsupportedMethod` for any other method
(including ones that are not in `StandardMethod`). 
what  `handleUnsupportedMethod` does from there will be explained in the section
on error handling below.

#### matching paths
`PathMatcher a` is a newtype for functions that take `PathConsumer`s and produce
`Maybe a`s. the `matchPath` functon uses these to choose the responding action
based on the current path state.

`PathMatcher` is a functor, which lets you do things like wrap inner actions
with other routing logic;
```haskell
-- this is provided in the module, though with a simpler definition
-- whatever action the original matcher would perform is now only performed if
-- the method matches.
matchPathWithMethod :: MonadRespond m => StdMethod -> PathMatcher (m ResponseReceived) -> PathMatcher (m ResponseReceived)
matchPathWithMethod method matcher = (matchMethod . onMethod method) <$> matcher
```

`PathMatcher` is also an instance of Applicative and, more importantly,
Alternative. the Alternative instance is what allows you to choose different
actions for different request paths, for instance

```haskell
-- for now, suppose this is at the top level of the api
pathExample0 :: RespondM ResponseReceived
pathExample0 = matchPath $
    -- if the request is to e.g. http://localhost:3000/, rootAction will produce the response
    pathEndOrSlash rootAction <|>
    -- however, if the request is to e.g. http://localhost:3000/one/ or http://localhost:3000/one, actionOne will get to respond
    pathLastSeg "one" actionOne
```

if the request is to none of those paths, then `handleUnmatchedPath` will get
called (see further on).

##### path extraction
obviously, we want to do more advanced matching on paths, and often we want to
get parameters out of paths. this is supported by the use of `PathExtractor`,
which is newtype wrapper around a somewhat fearsome-looking stack of monads. you
should look at the definition, but to summarize, it is meant to track
`PathConsumer` state , and possibly produce a value.

a `PathConsumer`, defined in `Web.Respond.Types.Path`, can be thought of as a
pair of the path segments that have been consumed and the path segments that
have not been consumed.
each field within the consumer keeps the segments in order, so it is possible to
rebuild the original request path using `getFullPath`. 
`pcConsumeNext` produces a new consumer with the first segment in the previous
unconsumed segment list appended to the consumed sequence (if there is no next
segment, i.e. `pcGetNext` produces nothing, `pcConsumeNext` should produce an
identical consumer).

the path extractor produced by `seg "whatever"` does the following when run
against a `PathMatcher` (e.g. by using the function `pathExtract`)
* it pulls out the current `PathConsumer` state
* if there is a next segment, and it matches the string "whatever", then it
  produces an empty value (`HList0`)
* it then updates the state with the result of applying `pcConsumeNext` to the
  old state.

these extractors can then be sequenced using the `(</>)` combinator, which takes
advantage of the applicative instance of `PathExtractor` to put them together.
(now, if you happen to be looking at the documentation for this function, you
might be wondering what all this HList stuff is about. we'll get to that soon.)

now you can use the `path` function to combine extractors and inner actions to
produce path matchers to use with `matchPath`. for example

```haskell
pathExample1 :: RespondM ResponseReceived
pathExample1 = matchPath $
    path (seg "one" </> seg "two" </> endOrSlash) someAction
```

and someAction will only get run if the request is to eg
`http://localhost:3000/one/two/`

it wouldn't make sense to call it `PathExtractor` if it couldn't extract values;
this is where HLists come in. a simple example is the `value` PathExtractor; it
produces a single value from a single segment if it can successfully be
extracted using the `PathPiece` instance. 
when multiple value extractors are chained, they build an HList of those types
(since `(</>)` appends each side's HList).
the `path` function's second parameter expects a function that takes the types
of the constructed HList and produces a MonadRespond action.
in fact, that is all that `HListElim` is; a function that has a type signature
that matches up with the types of an HList - the function `hListUncurry`
uncurries such a function against a conforming HList. putting this all together,
you can do something like

```haskell
pathInnerAction1 :: Integer -> Text -> Text -> RespondM ResponseReceived
pathInnerAction1 num t1 t2 = ... -- does whatever

pathExample2 :: RespondM ResponseReceived
pathExample2 = matchPath $
    path (seg seg "id" </> value </> seg "params" </> value </> value) pathInnerAction1
```

and e.g. a GET against `http://localhost:3000/id/42/params/one/two` would lead
to whatever response the action `pathInnerAction1 42 "one" "two"` produces.

##### path route nesting
an important point about the `path` function is that it runs the inner action
with a modified `PathConsumer` - specifically, the consumer produced by running
the `PathExtractor`. this is accomplished by building on the `withPath` function
specified by the the `MonadRespond` class. this lets you nest routing in a
sensible and hopefully pleasant way:

```haskell
idAction :: Text -> Integer -> RespondM ResponseReceived
idAction t i = --whatever

pathInnerRouting1 :: Text -> RespondM ResponseReceived
pathInnerRouting1 text = matchPath $
    pathEndOrSlash (innerRootAction text) <|>
    path (seg "id" </> value </> endOrSlash) (\v -> idAction text v)

pathExample3 :: RespondM ResponseReceived
pathExample3 = matchPath $
    pathEndOrSlash outerRootAction <|>
    path (seg "loc" </> value) pathInnerRouting1
```

which handles requests to e.g.
* `http://localhost:3000/` with `outerRootAction`
* `http://localhost:3000/seg/loc` with `handleUnmatchedPath`
* `http://localhost:3000/seg/loc/here` with `pathInnerRouting1 "here"`, which in
  turn uses `innerRootAction "here"`.
* `http://localhost:3000/seg/loc/here/id/55` with `pathInnerRouting1 "here"`, which in
  turn uses `idAction "here" 55`.

#### extracting request body values
todo

### generating responses
todo

#### ErrorReport and ReportableError
todo

#### handling failures and errors during request processing
todo


