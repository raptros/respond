{-|
Description: main module for library. exports everything.

= how to navigate the modules
this module re-exports all of the modules that make up this library, so
hopefully this can tell you where to find what you're looking for.

== fundamental modules

=== "Web.Respond.Types"
various types and classes are exported in this module

=== "Web.Respond.Monad"
the monad transformer and monadic interface for building a routing structure

== running a router

=== "Web.Respond.Run" 
contains functions that build WAI appliciations and run them in warp servers

=== "Web.Respond.DefaultServer"
defines the default setup for the Warp server 

== sending responses

=== "Web.Respond.Response"
functions for building responses using classes in "Web.Respond.Types" and sending them as 'MonadRespond' actions

=== "Web.Respond.DefaultHandlers"
defines the default set of 'RequestErrorHandlers' used by "Web.Respond.Run"

== processing and routing

=== "Web.Respond.Request"
contains request body processing tools and authentication/authorization tools

=== "Web.Respond.Method"
contains request method matching tools

=== "Web.Respond.Path"
contains path matching tools

-}
module Web.Respond (
                   module Web.Respond.Monad,
                   module Web.Respond.Types,
                   module Web.Respond.Run,
                   module Web.Respond.DefaultServer,
                   module Web.Respond.Response,
                   module Web.Respond.DefaultHandlers,
                   module Web.Respond.Request,
                   module Web.Respond.Method,
                   module Web.Respond.Path,
                   ) where

import Web.Respond.Types
import Web.Respond.Monad
import Web.Respond.Response
import Web.Respond.DefaultHandlers
import Web.Respond.Request
import Web.Respond.Method
import Web.Respond.Path
import Web.Respond.DefaultServer
import Web.Respond.Run
