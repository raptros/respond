{-|
Description: defines the 'FromBody' typeclass

defines the 'FromBody' typeclass and instances for

- lazy Text
- lazy extracted json
- strict extracted json
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Web.Respond.Types.Request where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Data.Aeson
import Data.Bifunctor

import Web.Respond.Types.Response
import Web.Respond.Types.Errors

-- | something that can be pulled from the body, restricted to
-- a ReportableError type.
class ReportableError e => FromBody e a | a -> e where
    -- | parse the body. note that the body is provided as a lazy
    -- ByteString. how that ByteString is loaded depends on the caller of
    -- fromBody.
    fromBody :: LBS.ByteString -> Either e a

-- * some instances

-- ** text
newtype TextBody = TextBody { getTextBody :: TL.Text } deriving (Eq, Show)

instance FromBody T.UnicodeException TextBody where
    fromBody = fmap TextBody . TL.decodeUtf8'

newtype TextBodyS = TextBodyS { getTextBodyS :: T.Text } deriving (Eq, Show)

instance FromBody T.UnicodeException TextBodyS where
    fromBody = fmap (TextBodyS . TL.toStrict) . TL.decodeUtf8'

-- ** JSON

-- | newtype for things that should be encoded as or parsed as Json.
-- 
-- the FromBody instance uses 'Data.Aeson.eitherDecode' - the lazy version.
newtype Json a = Json { getJson :: a }

instance FromJSON a => FromBody JsonParseError (Json a) where
    fromBody = bimap JsonParseError Json . eitherDecode

instance ToJSON a => ToResponseBody (Json a) where
    toResponseBody = matchAcceptJson . getJson

-- | newtype for things that should be encoded as or parsed as Json.
--
-- the 'FromBody' instance uses the immediate 'Data.Aeson.eitherDecode'' 
-- parser.
newtype JsonS a = JsonS { getJsonS :: a }

instance FromJSON a => FromBody JsonParseError (JsonS a) where
    fromBody = bimap JsonParseError JsonS . eitherDecode'

instance ToJSON a => ToResponseBody (JsonS a) where
    toResponseBody = matchAcceptJson . getJsonS

