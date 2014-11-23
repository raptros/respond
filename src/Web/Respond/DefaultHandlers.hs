{-|
Description: response utilities

utilities and defaults for sending responses.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Web.Respond.DefaultHandlers where

import Control.Applicative ((<$>))
import Network.Wai
import qualified Data.ByteString as BS
import Network.HTTP.Types.Status
--import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import qualified Data.Text as T
--import Control.Lens (view)

import Web.Respond.Types
import Web.Respond.Monad
import Web.Respond.Response

-- | default error handlers. uses the defaultXHandler for each _rehX.
defaultRequestErrorHandlers :: RequestErrorHandlers
defaultRequestErrorHandlers = RequestErrorHandlers {
    _rehUnsupportedMethod = defaultUnsupportedMethodHandler,
    _rehUnmatchedPath = defaultUnmatchedPathHandler,
    _rehPathParseFailed = defaultPathParseFailedHandler,
    _rehBodyParseFailed = defaultBodyParseFailureHandler,
    _rehAuthFailed = defaultAuthFailedHandler,
    _rehDenied = defaultDeniedHandler,
    _rehException = defaultExceptionHandler
}

-- | default unsupported method handler sends back an EmptyBody with status
-- 405 and an Allowed header listing the allowed methods in the first path
defaultUnsupportedMethodHandler :: MonadRespond m => [StdMethod] -> Method -> m ResponseReceived
defaultUnsupportedMethodHandler allowed = const $ respond $ EmptyBody methodNotAllowed405 [("Allowed", allowedStr allowed)]
    where allowedStr mths = BS.intercalate ", " (renderStdMethod <$> mths)

-- | respond with status404 and nothing else
defaultUnmatchedPathHandler :: MonadRespond m => m ResponseReceived
defaultUnmatchedPathHandler = respond (EmptyBody status404 [])

-- | respond with status 400 and the list of bad elements in the path
defaultPathParseFailedHandler :: MonadRespond m => [T.Text] -> m ResponseReceived
defaultPathParseFailedHandler failedOn = respond $ DefaultHeaders badRequest400 $ errorReportWithDetails "parse_error" failedOn

-- | respond with status 400 and a message about the body parse failure
defaultBodyParseFailureHandler :: MonadRespond m => ErrorReport -> m ResponseReceived
defaultBodyParseFailureHandler = respond . DefaultHeaders badRequest400

-- | respond with 401
defaultAuthFailedHandler :: MonadRespond m => ErrorReport -> m ResponseReceived
defaultAuthFailedHandler = respond . DefaultHeaders unauthorized401

-- | respond with 403
defaultDeniedHandler :: MonadRespond m => ErrorReport -> m ResponseReceived
defaultDeniedHandler = respond . DefaultHeaders forbidden403 

-- | respond with 500
defaultExceptionHandler :: MonadRespond m => ErrorReport -> m ResponseReceived
defaultExceptionHandler = respond . DefaultHeaders internalServerError500
