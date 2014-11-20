module Main where

import Data.Monoid ((<>))
import Control.Applicative ((<|>))
import Network.Wai
import Data.Aeson
import Network.HTTP.Types.Status
import qualified Data.Text as T
import Web.Respond
import Web.DefaultRespondServer
import System.Log.FastLogger


main :: IO ()
main = do
    logger <- newStdoutLoggerSet defaultBufSize 
    runWaiApp 3000 logger exampleApp

exampleApp :: Application
exampleApp = respondAppDefault id routeThang

type Example = RespondT IO

routeThang :: Example ResponseReceived
routeThang = matchPath $
    path endOrSlash apiRoot <|> 
    path (seg "assemble" ) assembleHandler <|>
    path (seg "number" </> value </> seg "string" </> value </> endOrSlash) numHandler
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
    onPUT (respond $ OkJson (object ["location" .= ("there" :: T.Text)]))

firstHandler :: T.Text -> Example ResponseReceived
firstHandler p = do 
    ps <- getUnconsumedPath
    respond $ DefaultHeaders notImplemented501 (object ["head" .= p, "tail" .= ps])

