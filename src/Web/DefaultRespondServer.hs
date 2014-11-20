module Web.DefaultRespondServer (runWaiApp) where

import Control.Applicative ((<$>))
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Data.Default.Class as Def
import System.Log.FastLogger

runWaiApp :: Warp.Port -> LoggerSet -> Application -> IO ()
runWaiApp port logger app = prepApp logger app >>= Warp.run port

prepApp :: LoggerSet -> Application -> IO Application
prepApp logger app = ($ app) <$> mkMiddleware logger

mkMiddleware :: LoggerSet -> IO Middleware
mkMiddleware logger = (. middlewares) <$> logMiddleware
    where
    middlewares = Gzip.gzip Def.def
    logMiddleware = RequestLogger.mkRequestLogger $ Def.def {
        RequestLogger.outputFormat = RequestLogger.Apache RequestLogger.FromSocket,
        RequestLogger.destination = RequestLogger.Logger logger
    }
