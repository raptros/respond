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

import Data.Monoid
import Control.Applicative
import Network.Wai
import Network.HTTP.Types.Method
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT, mapReaderT)
import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.List as List
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Cont as Cont
import qualified Control.Monad.Trans.State.Lazy as LazyState
import qualified Control.Monad.Trans.State.Strict as StrictState
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import Control.Monad.Reader.Class
import qualified Control.Monad.Cont.Class as Mtl
import qualified Control.Monad.Error.Class as Mtl
import qualified Control.Monad.State.Class as Mtl
import qualified Control.Monad.Writer.Class as Mtl
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import Control.Monad.Trans.Control (MonadTransControl, StT, liftWith, restoreT, defaultLiftWith, defaultRestoreT, MonadBaseControl, StM, liftBaseWith, defaultLiftBaseWith, restoreM, defaultRestoreM, ComposeSt)
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Monad.Catch

import Control.Lens ((%~), (.~), makeLenses, view)
import Web.Respond.Types
    
-- | this class is the api for building your handler.
class (Functor m, MonadIO m) => MonadRespond m where
    -- | perform the WAI application respond action (after converting the
    -- value to a response)
    respond :: Response -> m ResponseReceived
    -- | get out the request.
    getRequest :: m Request
    -- | run the inner route with a modified request
    withRequest :: Request -> m a -> m a
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

#define MRESPDEF(mapfun) {\
    respond = lift . respond; \
    getRequest = lift getRequest; \
    withRequest = mapfun . withRequest; \
    getHandlers = lift getHandlers; \
    withHandlers = mapfun . withHandlers; \
    getPath = lift getPath; \
    withPath = mapfun . withPath; \
    }

instance MonadRespond m => MonadRespond (ReaderT a m) where MRESPDEF(mapReaderT)

instance MonadRespond m => MonadRespond (Identity.IdentityT m) where MRESPDEF(Identity.mapIdentityT)

instance MonadRespond m => MonadRespond (Maybe.MaybeT m) where MRESPDEF(Maybe.mapMaybeT)

instance MonadRespond m => MonadRespond (List.ListT m) where MRESPDEF(List.mapListT)

instance MonadRespond m => MonadRespond (Except.ExceptT e m) where MRESPDEF(Except.mapExceptT)

instance MonadRespond m => MonadRespond (Cont.ContT r m) where MRESPDEF(Cont.mapContT)

instance MonadRespond m => MonadRespond (LazyState.StateT s m) where MRESPDEF(LazyState.mapStateT)

instance MonadRespond m => MonadRespond (StrictState.StateT s m) where MRESPDEF(StrictState.mapStateT)

instance (MonadRespond m, Monoid w) => MonadRespond (LazyWriter.WriterT w m) where MRESPDEF(LazyWriter.mapWriterT)

instance (MonadRespond m, Monoid w) => MonadRespond (StrictWriter.WriterT w m) where MRESPDEF(StrictWriter.mapWriterT)

instance (Monoid w, MonadRespond m) => MonadRespond (LazyRWS.RWST r w s m) where MRESPDEF(LazyRWS.mapRWST)

instance (Monoid w, MonadRespond m) => MonadRespond (StrictRWS.RWST r w s m) where MRESPDEF(StrictRWS.mapRWST)

rmapLoggingT :: (m a -> n b) -> LoggingT m a -> LoggingT n b
rmapLoggingT f = LoggingT . (f .) . runLoggingT

rmapNoLoggingT :: (m a -> n b) -> NoLoggingT m a -> NoLoggingT n b
rmapNoLoggingT f = NoLoggingT . f . runNoLoggingT

instance MonadRespond m => MonadRespond (LoggingT m) where MRESPDEF(rmapLoggingT)

instance MonadRespond m => MonadRespond (NoLoggingT m) where MRESPDEF(rmapNoLoggingT)


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
    withRequest r = local (request .~ r)
    getHandlers = view handlers
    withHandlers h = local (handlers %~ h)
    getPath = view pathConsumer
    withPath f = local (pathConsumer %~ f) 

runRespondTBase :: RespondT m a -> RespondData -> m a
runRespondTBase = runReaderT . unRespondT

-- | run the RespondT action with failure handlers and request information.
runRespondT :: RespondT m a -> FailureHandlers -> Request -> Responder -> m a
runRespondT act h req res = runRespondTBase act $ RespondData h req res (mkPathConsumer $ pathInfo req)

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

instance Mtl.MonadWriter w m => Mtl.MonadWriter w (RespondT m) where
    writer = lift . Mtl.writer
    tell = lift . Mtl.tell
    listen = mapRespondT Mtl.listen
    pass = mapRespondT Mtl.pass

instance Mtl.MonadError e m => Mtl.MonadError e (RespondT m) where
    throwError = lift . Mtl.throwError
    catchError m h = RespondT $ Mtl.catchError (unRespondT m) (unRespondT . h)

instance Mtl.MonadCont m => Mtl.MonadCont (RespondT m) where
    callCC = liftCallCC Mtl.callCC 
        where
        liftCallCC callCC f = RespondT $ ReaderT $ \ r -> callCC $ \ c -> runRespondTBase (f (RespondT . ReaderT . const . c)) r

instance Mtl.MonadState s m => Mtl.MonadState s (RespondT m) where
    get = lift Mtl.get
    put = lift . Mtl.put
    state = lift . Mtl.state

instance MonadLogger m => MonadLogger (RespondT m) where
    monadLoggerLog loc src level msg = lift $ monadLoggerLog loc src level msg

