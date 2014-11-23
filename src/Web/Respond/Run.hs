{-|
Description: building and running a RespondT app.

contains the tools to build and run a RespondT app
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Web.Respond.Run where

import Network.Wai
import Control.Monad.IO.Class (MonadIO)

import Web.Respond.Monad
import Web.Respond.DefaultHandlers
import Web.Respond.DefaultServer
import qualified Network.Wai.Handler.Warp as Warp
import System.Log.FastLogger (LoggerSet)

-- | build an 'Network.Wai.Application' from a 'RespondT' based handler.
respondApp :: MonadIO m  => RequestErrorHandlers -- ^ however you want errors handled
              -> (forall a. m a -> IO a) -- ^ how to unpeel your monad to 'IO'
              -> RespondT m ResponseReceived -- ^ your handler.
              -> Application -- ^ give this to warp or something
respondApp handlers lifter api req res = lifter (runRespondT api handlers req res)

-- | it's 'respondApp' with 'defaultRequestErrorHandlers' passed in.
respondAppDefault :: MonadIO m => (forall a. m a -> IO a)  -> RespondT m ResponseReceived -> Application 
respondAppDefault = respondApp defaultRequestErrorHandlers

-- | serve a RespondT app using 'runWaiApp' on 'respondApp'.
serveRespond :: MonadIO m => Warp.Port -> LoggerSet -> RequestErrorHandlers -> (forall a. m a -> IO a) -> RespondT m ResponseReceived -> IO ()
serveRespond port loggerSet handlers lifter api = runWaiApp port loggerSet (respondApp handlers lifter api)

-- | serve a RespondT app using 'runWaiApp' on 'respondAppDefault'
serveRespondDefault :: MonadIO m => Warp.Port -> LoggerSet -> (forall a. m a -> IO a) -> RespondT m ResponseReceived -> IO ()
serveRespondDefault port loggerSet lifter api = runWaiApp port loggerSet (respondAppDefault lifter api)
