{-|
Description: base types 

contains basic types used in Respond, and some functions for working with those types.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Web.Respond.Types where

import Network.Wai
import qualified Data.Text as T

import Control.Monad.Trans.State
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence as S
import Data.Aeson
import Data.String (fromString)

import Control.Lens (makeLenses, snoc, (%=), uses, (%~), _Left, _Right)
import Safe (headMay, tailSafe)

-- * responding

-- | instances of ToResponse can be converted into responses
class ToResponse a where
    toResponse :: a -> Response

instance ToResponse Response where
    toResponse = id

-- | the type of the responder callback that is handed to a WAI
-- 'Network.Wai.Application'
type Responder = Response -> IO ResponseReceived

-- * working with the path.

-- | stores the path and how much of it has been consumed
data PathConsumer = PathConsumer {
    -- | the consumed part of the path.
    _pcConsumed :: S.Seq T.Text,
    -- | the unconsumed part
    _pcUnconsumed :: [T.Text]
} deriving (Eq, Show)

makeLenses ''PathConsumer

-- | build a path consumer starting with nothing consumed
mkPathConsumer :: [T.Text] -> PathConsumer
mkPathConsumer = PathConsumer S.empty 

-- | get the next path element
pcGetNext :: PathConsumer -> Maybe T.Text
pcGetNext = headMay . _pcUnconsumed

-- | move forward in the path
pcConsumeNext :: PathConsumer -> PathConsumer
pcConsumeNext = execState $ do
    next <- uses pcUnconsumed headMay
    pcConsumed %= maybe id (flip snoc) next
    pcUnconsumed %= tailSafe

-- * errors

-- | an error report is something that can be sent back as a response
-- without having to worry about it too much.
data ErrorReport = ErrorReport {
    -- | the reason for the error. should describe the type of error that occurred to the api consumer.
    erReason :: T.Text,
    -- | a message that might explain why the error occurred.
    erMessage :: Maybe T.Text,
    -- | any details about the error that could be useful
    erDetails :: Maybe Value
}

-- | the ErrorReport json representation has the fields "reason",
-- "message", and "details". Absent message and details values are
-- represented as null in json.
instance ToJSON ErrorReport where
    toJSON er = object ["reason" .= erReason er, "message" .= erMessage er, "details" .= erDetails er]

-- | constructor for the simplest error report
simpleErrorReport :: T.Text -> ErrorReport
simpleErrorReport reason = ErrorReport reason Nothing Nothing

-- | constructor for error report with reason and message
errorReportWithMessage :: T.Text -> T.Text -> ErrorReport
errorReportWithMessage reason message = ErrorReport reason (Just message) Nothing

-- | constructor for error report with reason and details
errorReportWithDetails :: ToJSON d => T.Text -> d -> ErrorReport
errorReportWithDetails reason details = ErrorReport reason Nothing (Just $ toJSON details)

-- | error report with all the fixings
fullErrorReport :: ToJSON d => T.Text -> T.Text -> d -> ErrorReport
fullErrorReport reason message details = ErrorReport reason (Just message) (Just $ toJSON details)

-- | define how an error type should be reported.
-- you probably want to newtype when using this.
class ReportableError e where
    -- | you may way to use one of 'simpleErrorReport',
    -- 'errorReportWithMessage', 'errorReportWithDetails', or
    -- 'fullErrorReport', though of course it depends on the error type.
    toErrorReport :: e -> ErrorReport

instance ReportableError ErrorReport where
    toErrorReport = id

-- | wraps Aeson parse failure messages.
--
-- the 'ReportableError' instance uses "parse_failed" as the reason and the
-- error string from Aeson as the message.
newtype JsonParseError = JsonParseError String

instance ReportableError JsonParseError where
    toErrorReport (JsonParseError msg) = errorReportWithMessage "parse_failed" (fromString msg)

-- * working with the body

-- | something that can be pulled from the body, restricted to
-- a ReportableError type.
class ReportableError e => FromBody e a | a -> e where
    -- | parse the body. note that the body is provided as a lazy
    -- ByteString. how that ByteString is loaded depends on the caller of
    -- fromBody.
    fromBody :: LBS.ByteString -> Either e a

-- | newtype for parsing from a json body. the 'FromBody' instance uses the lazy 'eitherDecode' parser.
newtype Json a = Json { getJson :: a }

instance FromJSON a => FromBody JsonParseError (Json a) where
    fromBody = (_Left %~ JsonParseError) . (_Right %~ Json) . eitherDecode

