{-|
Description: types and tools for reporting errors

contains the ErrorReport data type and tools for constructing error reports, along with the ReportableError typeclass.
-}
{-# LANGUAGE RankNTypes #-}
module Web.Respond.Types.Errors where

import Data.Aeson
import Data.Aeson.Encode (encodeToTextBuilder)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Scientific as Sci
import Data.Int (Int64)
import Formatting
import Control.Applicative ((<$>), (<*>))
import Data.Vector ()
import Data.Bool (bool)
--import Control.Lens (lens, (%~))

import Network.HTTP.Types.Status
--import qualified Network.HTTP.Media as Media

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

-- ** rendering ErrorReports

-- | format a Status into a single string.
--
-- for example, "200 OK", or "404 Not Found"
statusFormat :: Format Status
statusFormat = later (bprint (int % now " " % stext) <$> statusCode <*> T.decodeUtf8 . statusMessage)

-- | i am not sure what the type means, but you pass this a default string
-- and a format for a thing, and it gives you a formatter for maybe that
-- thing.
maybeFormat :: forall m r a. m -> Holey TLB.Builder TLB.Builder (a -> m) -> Holey m r (Maybe a -> r)
maybeFormat x f = later (maybe x (bprint f))

-- *** format-builders

-- | build a format for an error report
errorReportFormat :: Format T.Text -- ^ format for the reason
                  -> Format T.Text -- ^ format for the message, if there is one
                  -> Format Value -- ^ format for the details, if any
                  -> Format ErrorReport
errorReportFormat reasonFmt messageFmt erdFmt = later (bprint (reasonFmt % maybeFormat "" messageFmt % maybeFormat "" erdFmt ) <$> erReason <*> erMessage <*> erDetails) 

boolFormat :: Format Bool
boolFormat = later $ bprint . bool "false" "true"

valueFold :: a -> (Bool -> a) -> (Sci.Scientific -> a) -> (T.Text -> a) -> (Array -> a) -> (Object -> a) -> Value -> a
valueFold f _ _ _ _ _ Null = f
valueFold _ f _ _ _ _ (Bool b) = f b
valueFold _ _ f _ _ _ (Number n) = f n
valueFold _ _ _ f _ _ (String s) = f s
valueFold _ _ _ _ f _ (Array a) = f a
valueFold _ _ _ _ _ f (Object o) = f o

data JContainer = JObj | JArr | JRoot

mkIndent :: Int64 -> TLB.Builder
mkIndent = TLB.fromLazyText . flip TL.replicate " "

-- | format a JSON value in a simple way. let Aeson handle the formatting.
simpleJsonValue :: Format Value
simpleJsonValue = later encodeToTextBuilder

-- *** formatters for plain text
-- | renders error report as plain text
renderPlainTextErrorReport :: Status -> ErrorReport -> TL.Text
renderPlainTextErrorReport = format $ statusFormat % "---\n" % errorReportFormat ("reason: " % stext % "\n") ("message: " % stext % "\n") ("details: " % simpleJsonValue % "\n")

pFormat :: Buildable a => Int64 -> Format a
pFormat indent = now (mkIndent indent) % "<p>" % build % "</p>\n"

htmlErrorReportFormat :: forall b. Holey TLB.Builder b (Status -> ErrorReport -> b)
htmlErrorReportFormat = "<!DOCTYPE html>\n" %
    "<html>\n" % 
    "  <head>\n" % 
    "    <title>Error</title>\n" %
    "  </head>\n" %
    "  <body>\n" %
    "    <h1>" % statusFormat % "</h1>\n" %
    errorReportFormat reasonFmt msgFmt detailsFmt % 
    "  </body>\n" %
    "</html>\n"
    where
    reasonFmt = pFormat 4 %. ("reason: " % stext)
    msgFmt = pFormat 4 %. ("message: " % stext)
    detailsFmt = pFormat 4 %. ("details: " % "<span>" % simpleJsonValue % "</span>")

-- | renders error report as HTML
renderHTMLErrorReport :: Status -> ErrorReport -> TL.Text
renderHTMLErrorReport = format $ htmlErrorReportFormat


-- * ReportableError class

-- | type class for responses that report errors.
class ReportableError e where
    reportError :: Status -- ^ the http error code that'll be sent
                -> e -- ^ the error to be reported
                -> BS.ByteString -- ^ the Accept header on the receiving end
                -> ResponseBody -- ^ the http body to send.

instance ReportableError ErrorReport where
    reportError status = matchToContentTypesDefault (textUtf8 "text/html" $ renderHTMLErrorReport status) [jsonMatcher, textUtf8 "text/plain" $ renderPlainTextErrorReport status]
