{-|
Description: response utilities

utilities and defaults for sending responses.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Web.Respond.Response where

import Control.Applicative ((<$>))
import Network.Wai
import qualified Data.ByteString as BS
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
--import qualified Data.Text as T
import Control.Lens (view)
import Control.Monad (join)
import Control.Monad.Catch
import Data.Maybe (fromMaybe)

import Web.Respond.Types
import Web.Respond.Monad


-- * headers
findHeader :: MonadRespond m => HeaderName -> m (Maybe BS.ByteString)
findHeader header = lookup header . requestHeaders <$> getRequest

findHeaderDefault :: MonadRespond m => HeaderName -> BS.ByteString -> m BS.ByteString
findHeaderDefault header defValue = fromMaybe defValue <$> findHeader header

-- | get the value of the Accept header, falling back to "*/*" if it was
-- not sent in the request
getAcceptHeader :: MonadRespond m => m BS.ByteString
getAcceptHeader = findHeaderDefault hAccept "*/*"

-- * constructing responses

-- | responding with an empty body means not having to worry about the
-- Accept header.
respondEmptyBody :: MonadRespond m => Status -> ResponseHeaders -> m ResponseReceived
respondEmptyBody status headers = respond $ responseLBS status headers ""

-- | respond by getting the information from a 'ResponseBody'
respondUsingBody :: MonadRespond m => Status -> ResponseHeaders -> ResponseBody -> m ResponseReceived
respondUsingBody status headers body = respond $ mkResponseForBody status headers body

-- | respond by using the ToResponseBody instance for the value and
-- determining if it can be converted into an acceptable response body.
--
-- calls 'handleUnacceptableResponse' if an acceptable content type cannot
-- be produced..
respondWith :: (MonadRespond m, ToResponseBody a) => Status -> ResponseHeaders -> a -> m ResponseReceived
respondWith status headers body = getAcceptHeader >>= maybe handleUnacceptableResponse respond . mkResponse status headers body

--mkResponse :: ToResponseBody a => Status -> ResponseHeaders -> a -> BS.ByteString -> Maybe Response

-- | respond with no additional headers
respondStdHeaders :: (MonadRespond m, ToResponseBody a) => Status -> a -> m ResponseReceived
respondStdHeaders = flip respondWith []

-- | respond with 200 Ok
respondOk :: (MonadRespond m, ToResponseBody a) => a -> m ResponseReceived
respondOk = respondStdHeaders ok200

-- | respond using a ReportableError to generate the response body.
respondReportError :: (MonadRespond m, ReportableError e) => Status -> ResponseHeaders -> e -> m ResponseReceived
respondReportError status headers err = getAcceptHeader >>= respondUsingBody status headers . reportError status err

-- | respond that something was not found
respondNotFound :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived
respondNotFound = respondReportError notFound404 []

-- * use the RequestErrorHandlers

-- | an action that gets the currently installed unsupported method handler
-- and applies it to the arguments
handleUnsupportedMethod :: MonadRespond m => [StdMethod] -> Method -> m ResponseReceived
handleUnsupportedMethod supported unsupported = do
    handler <- getHandler (view unsupportedMethod)
    handler supported unsupported

-- | an action that gets the installed unmatched path handler and uses it
handleUnmatchedPath :: MonadRespond m => m ResponseReceived
handleUnmatchedPath = join (getHandler (view unmatchedPath))

-- | get and use handler for unacceptable response types
handleUnacceptableResponse :: MonadRespond m => m ResponseReceived
handleUnacceptableResponse = join (getHandler (view unacceptableResponse))

-- | generic handler-getter for things that use ErrorReports
useHandlerForReport :: (MonadRespond m, ReportableError e) 
                    => (FailureHandlers -> e -> m ResponseReceived) 
                    -- ^ a handler-getter that gets a handler that takes
                    -- an error report
                    -> e 
                    -- ^ the error
                    -> m ResponseReceived
useHandlerForReport getter e = do
    h <- getHandler getter 
    h e

-- | an action that gets the installed body parse failure handler and
-- applies it
handleBodyParseFailure :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived
handleBodyParseFailure = useHandlerForReport (view bodyParseFailed)

-- | get and use installed auth failed handler
handleAuthFailed :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived
handleAuthFailed = useHandlerForReport (view authFailed)

-- | get and use access denied handler
handleAccessDenied :: (ReportableError e, MonadRespond m) => e -> m ResponseReceived
handleAccessDenied = useHandlerForReport (view accessDenied)

-- | get and use handler for caught exceptions.
handleCaughtException :: (ReportableError e, MonadRespond m) => e -> m ResponseReceived
handleCaughtException = useHandlerForReport (view caughtException)

-- | get a specific handler.
--
-- > getHandler = (<$> getHandlers)
getHandler :: MonadRespond m => (FailureHandlers -> a) -> m a
getHandler = (<$> getHandlers)

-- * other response utilities.

-- | a way to use Maybe values to produce 404s
maybeNotFound :: (ReportableError e, MonadRespond m) => e -> (a -> m ResponseReceived) -> Maybe a -> m ResponseReceived
maybeNotFound = maybe . respondReportError notFound404 []

-- | catch Exceptions using MonadCatch, and use 'handleCaughtException' to
-- respond with an error report.
catchRespond :: (MonadCatch m, MonadRespond m, ReportableError r, Exception e) => (e -> r) -> m ResponseReceived -> m ResponseReceived
catchRespond = handle . (handleCaughtException .)
