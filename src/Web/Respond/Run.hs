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

-- * using RespondT

-- | build an 'Network.Wai.Application' from a 'RespondT' router stack.
respondApp :: MonadIO m  => RequestErrorHandlers -- ^ however you want errors handled
              -> (forall a. m a -> IO a) -- ^ how to unpeel your monad to 'IO'
              -> RespondT m ResponseReceived -- ^ your handler - must respond.
              -> Application -- ^ give this to warp or something
respondApp handlers lifter api req res = lifter (runRespondT api handlers req res)

-- | it's 'respondApp' with 'defaultRequestErrorHandlers' passed in.
respondAppDefault :: MonadIO m => (forall a. m a -> IO a)  -> RespondT m ResponseReceived -> Application 
respondAppDefault = respondApp defaultRequestErrorHandlers

-- | serve a RespondT router app using 'runWaiApp' on 'respondApp'.
serveRespond :: MonadIO m => Warp.Port -> LoggerSet -> RequestErrorHandlers -> (forall a. m a -> IO a) -> RespondT m ResponseReceived -> IO ()
serveRespond port loggerSet handlers lifter api = runWaiApp port loggerSet (respondApp handlers lifter api)

-- | serve a RespondT router app using 'runWaiApp' on 'respondAppDefault'
serveRespondDefault :: MonadIO m => Warp.Port -> LoggerSet -> (forall a. m a -> IO a) -> RespondT m ResponseReceived -> IO ()
serveRespondDefault port loggerSet lifter api = runWaiApp port loggerSet (respondAppDefault lifter api)

-- * the simpler Respond

-- | RespondT stacked on top of IO; the simplest stack for handlers.
type RespondM a = RespondT IO a

-- | build an Application out of a RespondM handler.
simpleRespondApp :: RequestErrorHandlers -> RespondM ResponseReceived -> Application
simpleRespondApp handlers = respondApp handlers id 

-- | build an Application out of a RespondM handler using the default error handlers
simpleRespondAppDefault :: RespondM ResponseReceived -> Application
simpleRespondAppDefault = respondAppDefault id

-- | serve a RespondM handler
serveSimpleRespond :: Warp.Port -> LoggerSet -> RequestErrorHandlers -> RespondM ResponseReceived -> IO ()
serveSimpleRespond port loggerSet handlers = serveRespond port loggerSet handlers id 

-- | serve a RespondM handler using the default error handlers
serveSimpleRespondDefault :: Warp.Port -> LoggerSet -> RespondM ResponseReceived -> IO ()
serveSimpleRespondDefault port loggerSet = serveRespondDefault port loggerSet id 
