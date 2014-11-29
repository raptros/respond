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
import qualified Data.Text as T
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

-- * constructing responses

-- | responding with an empty body means not having to worry about the
-- Accept header.
respondEmptyBody :: MonadRespond m => Status -> ResponseHeaders -> m ResponseReceived
respondEmptyBody status headers = respond $ responseLBS status headers ""

-- | respond that the request's accept header could not be satisfied
respondUnacceptable :: MonadRespond m => m ResponseReceived
respondUnacceptable = respondEmptyBody notAcceptable406 []

-- | respond by getting the information from a 'ResponseBody'
respondUsingBody :: MonadRespond m => Status -> ResponseHeaders -> ResponseBody -> m ResponseReceived
respondUsingBody status headers body = respond $ mkResponseForBody status headers body

-- | respond by using the ToResponseBody instance for the value and
-- determining 
respondWith :: (MonadRespond m, ToResponseBody a) => Status -> ResponseHeaders -> a -> m ResponseReceived
respondWith status headers body = findHeaderDefault hAccept "*/*" >>= maybe respondUnacceptable respond . mkResponse status headers body

--mkResponse :: ToResponseBody a => Status -> ResponseHeaders -> a -> BS.ByteString -> Maybe Response

-- | respond with no additional headers
respondStdHeaders :: (MonadRespond m, ToResponseBody a) => Status -> a -> m ResponseReceived
respondStdHeaders = flip respondWith []

-- | respond with 200 Ok
respondOk :: (MonadRespond m, ToResponseBody a) => a -> m ResponseReceived
respondOk = respondStdHeaders ok200

-- | respond by using a reportable error as the body
respondReportableError :: (MonadRespond m, ReportableError e) => Status -> e -> m ResponseReceived
respondReportableError status err = respondStdHeaders status $ toErrorReport err

-- * use the RequestErrorHandlers

-- | an action that gets the currently installed unsupported method handler
-- and applies it to the arguments
handleUnsupportedMethod :: MonadRespond m => [StdMethod] -> Method -> m ResponseReceived
handleUnsupportedMethod supported unsupported = getREH (view rehUnsupportedMethod) >>= \handler -> handler supported unsupported

-- | an action that gets the installed unmatched path handler and uses it
handleUnmatchedPath :: MonadRespond m => m ResponseReceived
handleUnmatchedPath = join (getREH (view rehUnmatchedPath))

-- | an action that gets the installed path parse failed handler and
-- applies it
handlePathParseFailed :: MonadRespond m => [T.Text] -> m ResponseReceived
handlePathParseFailed parts = getREH (view rehPathParseFailed) >>= \handler -> handler parts

-- | generic handler-getter for things that use ErrorReports
useHandlerForReport :: (MonadRespond m, ReportableError e) => (RequestErrorHandlers -> ErrorReport -> m ResponseReceived) -> e -> m ResponseReceived
useHandlerForReport getter e = getREH getter >>= \handler -> handler (toErrorReport e)

-- | an action that gets the installed body parse failure handler and
-- applies it
handleBodyParseFailure :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived
handleBodyParseFailure = useHandlerForReport (view rehBodyParseFailed)

-- | get and use installed auth failed handler
handleAuthFailed :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived
handleAuthFailed = useHandlerForReport (view rehAuthFailed)

-- | get and use denied handler
handleDenied :: (ReportableError e, MonadRespond m) => e -> m ResponseReceived
handleDenied = useHandlerForReport (view rehDenied)

-- | get and use handler for caught exceptions.
handleException :: (ReportableError e, MonadRespond m) => e -> m ResponseReceived
handleException = useHandlerForReport (view rehException)


-- | get a specific handler.
--
-- > getREH = (<$> getREHs)
getREH :: MonadRespond m => (RequestErrorHandlers -> a) -> m a
getREH = (<$> getREHs)

-- * other response utilities.

-- | a way to use Maybe values to produce 404s
maybeNotFound :: (ReportableError e, MonadRespond m) => e -> (a -> m ResponseReceived) -> Maybe a -> m ResponseReceived
maybeNotFound = maybe . respondReportableError notFound404

-- | catch Exceptions using MonadCatch, and use 'handleException' to
-- respond with an error report.
catchRespond :: (MonadCatch m, MonadRespond m, ReportableError r, Exception e) => (e -> r) -> m ResponseReceived -> m ResponseReceived
catchRespond = handle . (handleException .)
