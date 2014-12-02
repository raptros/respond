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
alias stacks `RespondT` directly on `IO`.

(examples needed)

### running the app
there are several functions in `Web.Respond.Run` that you can use. the key ones
build a WAI application from a `RespondT` route (i.e. a value of type `RespondT m
ResponseReceived`); note that these functions (`respondApp` and `respondAppDefault`)
require a function to run the rest of the monadic stack to an `IO` value.
(the `RespondM` functions, of course, do not require this run function - running
the `RespondT` already produces the necessary `IO` value.)

the default handlers used by the \*Default functions are contained in
`Web.Respond.DefaultHandlers`; the default warp server setup used by the serve\*
functions are contained in `Web.Respond.DefaultServer.

(examples needed)

### request processing and routing
a number of tools are provided for (hopefully) convenient request processing and
routing.

#### matching methods
`Web.Respond.Method` defines the type `MethodMatcher`, which is just a newtype
around a Map from standard method to value. a monoid instance is defined for
this matcher type; you should use this instance and the `(<>)` function to
build up multi-method matchers

`matchMethod` takes a `MethodMatcher` that will produce an appropriate responding
action for each mapped HTTP method, and chooses the action that handles the
current request's method. 
when the given method matcher does not define an action for the current request
method, `matchMethod` calls `handleUnsupportedMethod` to provide the response
action; see the section on request error handlers further down.

(examples needed)

#### matching paths
todo


