module Main where

import Data.Monoid ((<>))
import Control.Applicative ((<|>))
import Network.Wai
import Data.Aeson
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import qualified Data.Text as T
import Web.Respond
import System.Log.FastLogger
import Control.Exception


main :: IO ()
main = do
    logger <- newStdoutLoggerSet defaultBufSize 
    serveRespondDefault 3000 logger id exampleApi

type Example = RespondT IO

exampleApi :: Example ResponseReceived
exampleApi = matchPath $
    path endOrSlash apiRoot <|> 
    path (seg "assemble" ) assembleHandler <|>
    path (seg "number" </> value </> seg "string" </> value </> endOrSlash) numHandler <|>
    path (seg "error" </> endOrSlash) errorExample <|>
    path (seg "methoding") methodingExample
    where
    numHandler :: Integer -> T.Text -> Example ResponseReceived
    numHandler num t = respond $ OkJson $ object ["text" .= t, "num" .= num]

assembleHandler :: Example ResponseReceived
assembleHandler = matchPath $
    path endOrSlash (respond $ DefaultHeaders notImplemented501 (object ["awaiting" .= ("stray kitten" :: T.Text)])) <|>
    path value firstHandler

apiRoot :: Example ResponseReceived
apiRoot = matchMethod $
    onGET (respond $ OkJson (object ["location" .= ("here" :: T.Text)])) <>
    onPUT (respond $ OkJson (object ["location" .= ("there" :: T.Text)])) <>
    onPOST (withRequiredBody $ showPostedValue . getJson)
    where
    showPostedValue :: Value -> Example ResponseReceived
    showPostedValue v = respond $ OkJson ["location" .= ("cold" :: T.Text), "recvd" .= v]

firstHandler :: T.Text -> Example ResponseReceived
firstHandler p = do 
    ps <- getUnconsumedPath
    respond $ DefaultHeaders notImplemented501 (object ["head" .= p, "tail" .= ps])

errorExample :: Example ResponseReceived
errorExample = catchRespond displayError $ matchMethod $
    onGET $ error "hey what's going on"
    where
    displayError (ErrorCall s) = errorReportWithMessage "error_thrown" (T.pack s)

textGET :: T.Text
textGET = "get"

textPOST :: T.Text
textPOST = "post"

textOne :: T.Text
textOne = "one"

textTwo :: T.Text
textTwo = "two"

methodingExample :: Example ResponseReceived
methodingExample = matchPath $
    pathEndOrSlash (matchMethod $
        onGET (respond $ OkJson $ object ["method" .= textGET]) <>
        onPOST (respond $ OkJson $ object ["method" .= textPOST])) <|>
    pathWithMethod GET (
        pathLastSeg "one" (respond $ OkJson $ object ["got" .= textOne]) <|>
        path (seg "two" </> (value :: PathExtractor1 Int)) (\i -> respond $ OkJson $ object ["got" .= textTwo, "v" .= i]))

