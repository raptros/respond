{-|
Description: types and tools for making responses

types and tools for making responses
-}
{-# LANGUAGE TupleSections #-}
module Web.Respond.Types.Response where

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import qualified Network.HTTP.Media as Media
import Network.Wai

import Control.Applicative ((<$>), pure)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson

-- * responding

-- | the type of the responder callback that is handed to a WAI
-- 'Network.Wai.Application'
type Responder = Response -> IO ResponseReceived

-- * response bodies.

type ResponseBuilder = Status -> ResponseHeaders -> Response

-- | contains both the content type header value and the body.
type ResponseBody = (Media.MediaType, BSL.ByteString)

-- | instances of this class produce content-negotiated response bodies.
--
-- if you're in a situation where there are performance concerns around
-- building a lazy bytestring for the response, you should consider instead
-- building 'Response's by hand (i.e. by using the WAI functions).
class ToResponseBody a where
    -- | specify the conversion. this is supposed to fail if and only if
    -- there is no way for the instance to satisfy the accept header.
    -- otherwise. a successful conversion will include both a bytestring
    -- to use as the body and the value to use for the content type header
    toResponseBody :: a -- ^ the value to convert
                   -> BS.ByteString -- ^ the http accept header. will be "*/*" if the client didn't set it.
                   -> Maybe ResponseBody  -- ^ content type header, body


-- ** tools to build instances

-- | pair of media type to match and a function that produces a builder for
-- a value
type MediaTypeMatcher a = (Media.MediaType, a -> BSL.ByteString)

-- | converts a media type matcher into a pair of media type and response
-- body...
--
-- yes, what it does is duplicate the media type and apply the builder to
-- the value
prepMediaTypeMatcher :: a -> MediaTypeMatcher a -> (Media.MediaType, ResponseBody)
prepMediaTypeMatcher v (mtype, builder) = (mtype, (mtype, builder v))

-- | find the media type matcher that matches the passed Accept header
-- value, and use it to produce a ResponseBuilder for the passed value.
-- fail with Nothing if no builder can be found given the header. see
-- 'prepMediaTypeMatcher'
matchToContentTypes :: [MediaTypeMatcher a] -> a -> BS.ByteString -> Maybe ResponseBody
matchToContentTypes matchers v = Media.mapAcceptMedia (prepMediaTypeMatcher v <$> matchers)

-- ** working with instances

-- | take a ResponseBody instance and turn it into a response
mkResponseForBody :: Status -> ResponseHeaders -> ResponseBody -> Response
mkResponseForBody status headers (mtype, body) = responseLBS status ((hContentType, Media.renderHeader mtype):headers) body

-- | try to produce a Response object for an instance of ToResponseBody.
-- the last input must be the value of the accept header
mkResponse :: ToResponseBody a => Status -> ResponseHeaders -> a -> BS.ByteString -> Maybe Response
mkResponse status headers val accept = mkResponseForBody status headers <$> toResponseBody val accept

-- * working with content types

-- ** JSON
-- | make a MediaTypeMatcher that produces JSON
jsonMatcher :: ToJSON a => MediaTypeMatcher a
jsonMatcher = ("application/json", encode)

-- | matches only if client accepts "application/json"
matchAcceptJson :: ToJSON a => a -> BS.ByteString -> Maybe ResponseBody
matchAcceptJson = matchToContentTypes [jsonMatcher]

instance ToResponseBody Value where
    toResponseBody = matchAcceptJson

-- ** renderable content types

-- *** HTML

-- | builds an html matcher
htmlMatcher :: (a -> BSL.ByteString) -> MediaTypeMatcher a
htmlMatcher = ("text/html",)

-- | matches only html acceptance
matchAcceptHtml :: (a -> BSL.ByteString) -> a -> BS.ByteString -> Maybe ResponseBody
matchAcceptHtml = matchToContentTypes . pure . htmlMatcher

-- ** plaintext

textPlainMatcher :: (a -> BSL.ByteString) -> MediaTypeMatcher a
textPlainMatcher = ("text/plain",)

matchTextPlain :: (a -> BSL.ByteString) -> a -> BS.ByteString -> Maybe ResponseBody
matchTextPlain = matchToContentTypes . pure . textPlainMatcher

