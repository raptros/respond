{-|
Description: default warp server setup

Provides a runner for a Warp server to run an app with some hopefully sensible middleware (such as request logging).
-}
module Web.Respond.DefaultServer where

import Control.Applicative ((<$>))
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Data.Default.Class as Def
import System.Log.FastLogger

-- | sets up the app using 'prepApp' then uses 'Warp.run' to run it.
runWaiApp :: Warp.Port -> LoggerSet -> Application -> IO ()
runWaiApp port logger app = prepApp logger app >>= Warp.run port

-- | combines the application with the middleware created by
-- 'mkMiddleware'
prepApp :: LoggerSet -> Application -> IO Application
prepApp logger app = ($ app) <$> mkMiddleware logger

-- | combines gzip middleware and request logging middleware
--
-- see 'Gzip.gzip'; uses the default values for it.
--
-- the request logger is set up with the format 'RequestLogger.Apache'
-- 'RequestLogger.FromSocket', and uses the 'LoggerSet' as the destination.
-- see 'RequestLogger.mkRequestLogger'.
mkMiddleware :: LoggerSet -> IO Middleware
mkMiddleware logger = (. middlewares) <$> logMiddleware
    where
    middlewares = Gzip.gzip Def.def
    logMiddleware = RequestLogger.mkRequestLogger $ Def.def {
        RequestLogger.outputFormat = RequestLogger.Apache RequestLogger.FromSocket,
        RequestLogger.destination = RequestLogger.Logger logger
    }
