{-|
Description: the monad and all of its support

you build your api using this stuff.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Web.Respond.Monad (
                         -- * the monad interface
                         MonadRespond(..),
                         -- ** an implementation
                         RespondT,
                         runRespondT,
                         mapRespondT,
                         -- * handling errors
                         FailureHandlers(..),
                         -- ** Getters for each handler
                         unsupportedMethod, 
                         unmatchedPath, 
                         bodyParseFailed,
                         authFailed,
                         accessDenied,
                         caughtException,
                         unacceptableResponse
                         ) where

import Control.Applicative
import Network.Wai
import Network.HTTP.Types.Method
import Control.Monad.Trans.Reader (ReaderT, runReaderT, mapReaderT)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Trans.Except (ExceptT, mapExceptT)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import Control.Monad.Trans.Control (MonadTransControl, StT, liftWith, restoreT, defaultLiftWith, defaultRestoreT, MonadBaseControl, StM, liftBaseWith, defaultLiftBaseWith, restoreM, defaultRestoreM, ComposeSt)
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Monad.Catch

import Control.Lens ((%~), makeLenses, view)
import Web.Respond.Types
    
-- | this class is the api for building your handler.
class (Functor m, MonadIO m) => MonadRespond m where
    -- | perform the WAI application respond action (after converting the
    -- value to a response)
    respond :: Response -> m ResponseReceived
    -- | get out the request.
    getRequest :: m Request
    -- | get the 'FailureHandlers'.
    getHandlers :: m FailureHandlers
    -- | run an inner action that will see an updates set of error
    -- handlers. this is useful when you know that inner actions will need
    -- to do resource cleanup or something.
    withHandlers :: (FailureHandlers -> FailureHandlers) -> m a -> m a
    -- | get the path as it's been consumed so far.
    getPath :: m PathConsumer
    -- | run the inner action with an updated path state.
    withPath :: (PathConsumer -> PathConsumer) -> m a -> m a

instance MonadRespond m => MonadRespond (ExceptT e m) where
    respond = lift . respond
    getRequest = lift getRequest
    getHandlers = lift getHandlers
    withHandlers = mapExceptT . withHandlers
    getPath = lift getPath
    withPath = mapExceptT . withPath

instance MonadRespond m => MonadRespond (MaybeT m) where
    respond = lift . respond
    getRequest = lift getRequest
    getHandlers = lift getHandlers
    withHandlers = mapMaybeT . withHandlers
    getPath = lift getPath
    withPath = mapMaybeT . withPath

-- | record containing responders that request matching tools can use when
-- failures occur.
data FailureHandlers = FailureHandlers {
    -- | what to do if the request method is not supported
    _unsupportedMethod :: MonadRespond m => [StdMethod] -> Method -> m ResponseReceived,
    -- | what to do if the request path has no matches
    _unmatchedPath :: MonadRespond m => m ResponseReceived,
    -- | what to do if the body failed to parse
    _bodyParseFailed :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived,
    -- | what to do when authentication fails
    _authFailed :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived,
    -- | what to do when authorization fails
    _accessDenied :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived,
    -- | what to do when an exception has been caught
    _caughtException :: (MonadRespond m, ReportableError e) => e -> m ResponseReceived,
    -- | what to do when no media type is acceptable
    _unacceptableResponse :: (MonadRespond m) => m ResponseReceived
}

makeLenses ''FailureHandlers

-- | this is the environment data used by RespondT. you probably don't want
-- to mess with this.
data RespondData = RespondData {
    _handlers :: FailureHandlers,
    _request :: Request,
    _responder :: Responder,
    _pathConsumer :: PathConsumer
}

makeLenses ''RespondData

-- | RespondT is a monad transformer that provides an implementation of
-- MonadRespond. you build your application using this.
newtype RespondT m a = RespondT { 
    unRespondT :: ReaderT RespondData m a 
} deriving (Functor, Applicative, Monad, MonadReader RespondData)

instance (Functor m, MonadIO m) => MonadRespond (RespondT m) where
    respond v = view responder >>= \r -> liftIO . r $ v
    getRequest = view request
    getHandlers = view handlers
    withHandlers h = local (handlers %~ h)
    getPath = view pathConsumer
    withPath f = local (pathConsumer %~ f) 

-- | run the RespondT action with failure handlers and request information.
runRespondT :: RespondT m a -> FailureHandlers -> Request -> Responder -> m a
runRespondT (RespondT act) h req res = runReaderT act $ RespondData h req res (mkPathConsumer $ pathInfo req)

mapRespondT :: (m a -> n b) -> RespondT m a -> RespondT n b
mapRespondT f = RespondT . mapReaderT f . unRespondT

instance MonadTrans RespondT where
    lift act = RespondT $ lift act

instance MonadIO m => MonadIO (RespondT m) where
    liftIO act = RespondT $ liftIO act

instance MonadThrow m => MonadThrow (RespondT m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (RespondT m) where
    catch act h = RespondT $ catch (unRespondT act) (\e -> unRespondT (h e))

--these next three son of a gun all need UndecidableInstances

instance MonadBase b m => MonadBase b (RespondT m) where
    liftBase = liftBaseDefault

-- and these two demand TypeFamilies

instance MonadTransControl RespondT where
#if MIN_VERSION_monad_control(1, 0, 0)
    type StT RespondT a = StT (ReaderT RespondData) a
    liftWith = defaultLiftWith RespondT unRespondT
    restoreT = defaultRestoreT RespondT
#else
    newtype StT RespondT a = StRespond { unStRespond :: StT (ReaderT RespondData) a }
    liftWith = defaultLiftWith RespondT unRespondT StRespond
    restoreT = defaultRestoreT RespondT unStRespond
#endif

instance MonadBaseControl b m => MonadBaseControl b (RespondT m) where
#if MIN_VERSION_monad_control(1, 0, 0)
    type StM (RespondT m) a = ComposeSt RespondT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
#else
    newtype StM (RespondT m) a = StMT { unStMT :: ComposeSt RespondT m a}
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM   unStMT
#endif

instance MonadLogger m => MonadLogger (RespondT m) where
    monadLoggerLog loc src level msg = lift $ monadLoggerLog loc src level msg

