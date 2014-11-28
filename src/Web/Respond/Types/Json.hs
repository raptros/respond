{-# LANGUAGE MultiParamTypeClasses #-}
module Web.Respond.Types.Json where

import Data.Aeson
import Data.Bifunctor
import Data.String (fromString)

import Web.Respond.Types.Response
import Web.Respond.Types.Errors
import Web.Respond.Types.Request

-- | wraps Aeson parse failure messages.
--
-- the 'ReportableError' instance uses "parse_failed" as the reason and the
-- error string from Aeson as the message.
newtype JsonParseError = JsonParseError String

instance ReportableError JsonParseError where
    toErrorReport (JsonParseError msg) = errorReportWithMessage "parse_failed" (fromString msg)

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
