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
--import Control.Lens (view)

import Web.Respond.Types
import Web.Respond.Monad
import Web.Respond.Response

-- | default failure handlers. uses the defaultXHandler for each field
defaultHandlers :: FailureHandlers
defaultHandlers = FailureHandlers {
    _unsupportedMethod = defaultUnsupportedMethodHandler,
    _unmatchedPath = defaultUnmatchedPathHandler,
    _bodyParseFailed = defaultBodyParseFailureHandler,
    _authFailed = defaultAuthFailedHandler,
    _accessDenied = defaultAccessDeniedHandler,
    _caughtException = defaultCaughtExceptionHandler,
    _unacceptableResponse = defaultUnacceptableResponseHandler
}

-- | default unsupported method handler sends back an EmptyBody with status
-- 405 and an Allowed header listing the allowed methods in the first path
defaultUnsupportedMethodHandler :: MonadRespond m => [StdMethod] -> Method -> m ResponseReceived
defaultUnsupportedMethodHandler allowed = const $ respondEmptyBody methodNotAllowed405 [("Allowed", allowedStr allowed)]
    where allowedStr mths = BS.intercalate ", " (renderStdMethod <$> mths)

-- | respond with status404 and nothing else
defaultUnmatchedPathHandler :: MonadRespond m => m ResponseReceived
defaultUnmatchedPathHandler = respondEmptyBody notFound404 []

-- | respond with status 400 and a message about the body parse failure
defaultBodyParseFailureHandler :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived
defaultBodyParseFailureHandler = respondReportError badRequest400 []

-- | respond with 401
defaultAuthFailedHandler :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived
defaultAuthFailedHandler = respondReportError unauthorized401 []

-- | respond with 403
defaultAccessDeniedHandler :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived
defaultAccessDeniedHandler = respondReportError forbidden403 []

-- | respond with 500
defaultCaughtExceptionHandler :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived
defaultCaughtExceptionHandler = respondReportError internalServerError500 []

-- | respond with 406
defaultUnacceptableResponseHandler :: MonadRespond m => m ResponseReceived
defaultUnacceptableResponseHandler = respondEmptyBody notAcceptable406 []
