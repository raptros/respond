{-|
Description: response utilities

utilities and defaults for sending responses.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Web.Respond.Response where

import Control.Applicative ((<$>))
import Network.Wai
import Data.Aeson
import qualified Data.ByteString as BS
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import qualified Data.Text as T
import Control.Lens (view)
import Control.Monad (join)

import Web.Respond.Types
import Web.Respond.Monad

-- | AsJson is a 'ToResponse' instance for sending JSON responses.
data AsJson a =
        -- | send a JSON response with a status and a set of headers to
        -- include
        AddHeaders Status ResponseHeaders a |
        -- | send a JSON response with a status but leave the headers alone
        DefaultHeaders Status a |
        -- | send a 200 response with JSON and no added headers
        OkJson a

instance ToJSON a => ToResponse (AsJson a) where
    toResponse (AddHeaders status hdrs a) = responseJson status hdrs a
    toResponse (DefaultHeaders status a) = responseJson status [] a
    toResponse (OkJson a) = responseJson ok200 [] a

-- | a 'ToResponse' instance that sends a status and headers with content
-- type set to json and no actual response body.
data EmptyJson = EmptyJson Status ResponseHeaders

instance ToResponse EmptyJson where
    toResponse (EmptyJson status hdrs) = responseJson status hdrs (object [])

-- | send a response with an empty body, content type json, and content
-- length 0.
data EmptyBody = EmptyBody Status ResponseHeaders

instance ToResponse EmptyBody where
    toResponse (EmptyBody status hdrs) = responseLBS status (headerCTJson : (hContentLength, "0") : hdrs) ""

-- | use a ReportableError as a response
data ResponseError e = ResponseError Status e

instance ReportableError e => ToResponse (ResponseError e) where
    toResponse (ResponseError status e) = responseJson status [] (toErrorReport e)

-- | the bytestring "application/json"
contentTypeJson :: BS.ByteString
contentTypeJson = "application/json"

-- | make a Content-Type: header
mkContentType :: BS.ByteString -> Header
mkContentType = (hContentType, )

-- | "Content-Type: application/json"
headerCTJson :: Header
headerCTJson = mkContentType contentTypeJson

-- | build a response object with content type json and the value as a json
-- body
responseJson :: ToJSON a => Status -> ResponseHeaders -> a -> Response
responseJson s hs a = responseLBS s (headerCTJson : hs) (encode a)

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

-- | an action that gets the installed body parse failure handler and
-- applies it
handleBodyParseFailure :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived
handleBodyParseFailure e = getREH (view rehBodyParseFailed) >>= \handler -> handler (toErrorReport e)

-- | get and use installed auth failed handler
handleAuthFailed :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived
handleAuthFailed e = getREH (view rehAuthFailed) >>= \handler -> handler (toErrorReport e)

-- | get and use denied handler
handleDenied :: (ReportableError e, MonadRespond m) => e -> m ResponseReceived
handleDenied e = getREH (view rehDenied) >>= \handler -> handler (toErrorReport e)

-- | get a specific handler.
--
-- > getREH = (<$> getREHs)
getREH :: MonadRespond m => (RequestErrorHandlers -> a) -> m a
getREH = (<$> getREHs)
