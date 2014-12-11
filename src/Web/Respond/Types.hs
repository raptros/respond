{-|
Description: base types 

= navigating the types modules
there are a bunch of type-defining modules here; hopefully you can find what you want

== "Web.Respond.Types.Path"
this module defines 'PathConsumer' and several functions for working with that type

== "Web.Respond.Types.Response"
defines the types 'Responder', 'ResponseBody', and 'MediaTypeMatcher'; also defines the typeclass 'ToResponseBody'.
provides tools for implementing instances of the 'ToResponseBody' by matching against Accept headerst

== "Web.Respond.Types.Errors"
defines the typeclass 'ReportableError', similar to 'ToResponseBody' except with a fallback 'ResponseBody' when unable to match the Accept header.
also defines the 'ErrorReport' datatype, and implements 'ReportableError' for it, defining the formats for rendering it to a response.
provides an instance of 'ReportableError' for unicode errors.

== "Web.Respond.Types.Request"
defines the 'FromBody' typeclass along with
- 'TextBody' newtype, with an appropriate 'FromBody' instance
- 'Json' and 'JsonS' newtypes, with appropriate 'FromBody' and 'ToResponseBody' instances

-}
module Web.Respond.Types (
                        module Web.Respond.Types.Path,
                        module Web.Respond.Types.Response,
                        module Web.Respond.Types.Errors,
                        module Web.Respond.Types.Request
                        ) where

import Web.Respond.Types.Path
import Web.Respond.Types.Response
import Web.Respond.Types.Request
import Web.Respond.Types.Errors
