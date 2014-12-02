{-|
Description: newtypes for working with json in requests and responses

defines newtypes 'Json' and 'JsonS' for decoding request bodies as JSON (lazy vs strict) and encoding responses as json
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web.Respond.Types.Json where

import Data.Aeson
import Data.Bifunctor
import Data.String (fromString)

import Web.Respond.Types.Response
import Web.Respond.Types.Errors
import Web.Respond.Types.Request

-- | convert a parse failure string message into an 'ErrorReport'; this
-- gets used to implement 'FromBody' for 'Json' and 'JsonS'
reportJsonParseError :: String -> ErrorReport
reportJsonParseError msg = errorReportWithMessage "parse_failed" (fromString msg)

-- | newtype for things that should be encoded as or parsed as Json.
-- 
-- the FromBody instance uses 'Data.Aeson.eitherDecode' - the lazy version.
newtype Json a = Json { getJson :: a }

instance FromJSON a => FromBody ErrorReport (Json a) where
    fromBody = bimap reportJsonParseError Json . eitherDecode

instance ToJSON a => ToResponseBody (Json a) where
    toResponseBody = matchAcceptJson . getJson

-- | newtype for things that should be encoded as or parsed as Json.
--
-- the 'FromBody' instance uses the immediate 'Data.Aeson.eitherDecode'' 
-- parser.
newtype JsonS a = JsonS { getJsonS :: a }

instance FromJSON a => FromBody ErrorReport (JsonS a) where
    fromBody = bimap reportJsonParseError JsonS . eitherDecode'

instance ToJSON a => ToResponseBody (JsonS a) where
    toResponseBody = matchAcceptJson . getJsonS

