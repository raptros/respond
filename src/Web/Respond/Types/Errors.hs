{-|
Description: types and tools for reporting errors

contains the ErrorReport data type and tools for constructing error reports, along with the ReportableError typeclass.
-}
module Web.Respond.Types.Errors where

import Data.Aeson
import qualified Data.Text as T

import Web.Respond.Types.Response

-- * ErrorReport data
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

--- instances below
instance ToResponseBody ErrorReport where
    toResponseBody = matchToContentTypes [jsonMatcher]

-- ** building error reports

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

-- * ReportableError class

-- | define how an error type should be reported.
-- you probably want to newtype when using this.
class ReportableError e where
    -- | you may way to use one of 'simpleErrorReport',
    -- 'errorReportWithMessage', 'errorReportWithDetails', or
    -- 'fullErrorReport', though of course it depends on the error type.
    toErrorReport :: e -> ErrorReport

instance ReportableError ErrorReport where
    toErrorReport = id
