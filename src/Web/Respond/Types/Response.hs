{-|
Description: types and tools for making responses

types and tools for making responses
-}
module Web.Respond.Types.Response where

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import qualified Network.HTTP.Media as Media
import Network.Wai

import Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import Data.Aeson

-- * responding

-- | the type of the responder callback that is handed to a WAI
-- 'Network.Wai.Application'
type Responder = Response -> IO ResponseReceived

-- * response bodies.

type ResponseBuilder = Status -> ResponseHeaders -> Response

-- | class produces an Acceptable response body
--
-- todo: determine whether or not this can be simplified to return either
-- a bytestring or a builder
class ToResponseBody a where
    -- | specify the conversion. this is supposed to fail if and only if
    -- there is no way for the instance to satisfy the accept header.
    -- otherwise. otherwise it must produce a function that creates
    -- a response with the passed in status and headers. (this function is
    -- allowed to add content type, though.)
    toResponseBody :: a -- ^ the value to convert
                   -> BS.ByteString -- ^ the http accept header. will be "*/*" if the client didn't pass one
                   -> Maybe ResponseBuilder -- ^ possibly a producer of responses

-- ** tools to build instances

-- | pair of media type to match and a function that produces a builder for
-- a value
type MediaTypeMatcher a = (Media.MediaType, a -> ResponseBuilder)

-- | converts a media type matcher into a pair of media type and response
-- builder. the produced response builder calls the input builder with the
-- given value and a headers list with the content type header prepended.
prepMediaTypeMatcher :: a -> MediaTypeMatcher a -> (Media.MediaType, ResponseBuilder)
prepMediaTypeMatcher v (mtype, builder) = (mtype, builder')
    where
    contentTypeHeader = (hContentType, Media.renderHeader mtype)
    builder' status headers = builder v status (contentTypeHeader:headers)

-- | find the media type matcher that matches the passed Accept header
-- value, and use it to produce a ResponseBuilder for the passed value.
-- fail with Nothing if no builder can be found given the header. see
-- 'prepMediaTypeMatcher'
matchToContentTypes :: [MediaTypeMatcher a] -> a -> BS.ByteString -> Maybe ResponseBuilder
matchToContentTypes matchers v = Media.mapAcceptMedia (prepMediaTypeMatcher v <$> matchers)


-- * Working with JSON
-- | make a MediaTypeMatcher that produces JSON
jsonMatcher :: ToJSON a => MediaTypeMatcher a
jsonMatcher = ("application/json", \v s h -> responseLBS s h (encode v))

-- | matches only if client accepts "application/json"
matchAcceptJson :: ToJSON a => a -> BS.ByteString -> Maybe ResponseBuilder
matchAcceptJson = matchToContentTypes [jsonMatcher]

instance ToResponseBody Value where
    toResponseBody = matchAcceptJson
