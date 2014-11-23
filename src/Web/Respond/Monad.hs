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
module Web.Respond.Monad (
                         -- * the monad interface
                         MonadRespond(..),
                         -- ** an implementation
                         RespondT,
                         runRespondT,
                         mapRespondT,
                         -- * handling errors
                         RequestErrorHandlers(..),
                         -- ** Getters for each handler
                         rehUnsupportedMethod, 
                         rehUnmatchedPath, 
                         rehPathParseFailed,
                         rehBodyParseFailed,
                         rehAuthFailed,
                         rehDenied,
                         rehException
                         ) where

import Control.Applicative
import Network.Wai
import Network.HTTP.Types.Method
import qualified Data.Text as T
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
    respond :: ToResponse v => v -> m ResponseReceived
    -- | get out the request.
    getRequest :: m Request
    -- | get the 'RequestErrorHandlers'.
    getREHs :: m RequestErrorHandlers
    -- | run an inner action that will see an updates set of error
    -- handlers. this is useful when you know that inner actions will need
    -- to do resource cleanup or something.
    withREHs :: (RequestErrorHandlers -> RequestErrorHandlers) -> m a -> m a
    -- | get the path as it's been consumed so far.
    getPath :: m PathConsumer
    -- | run the inner action with an updated path state.
    withPath :: (PathConsumer -> PathConsumer) -> m a -> m a

instance MonadRespond m => MonadRespond (ExceptT e m) where
    respond = lift . respond
    getRequest = lift getRequest
    getREHs = lift getREHs
    withREHs = mapExceptT . withREHs
    getPath = lift getPath
    withPath = mapExceptT . withPath

instance MonadRespond m => MonadRespond (MaybeT m) where
    respond = lift . respond
    getRequest = lift getRequest
    getREHs = lift getREHs
    withREHs = mapMaybeT . withREHs
    getPath = lift getPath
    withPath = mapMaybeT . withPath

-- | record containing responders that request matching tools can use when
-- failures occur.
data RequestErrorHandlers = RequestErrorHandlers {
    -- | what to do if the request method is not supported
    _rehUnsupportedMethod :: MonadRespond m => [StdMethod] -> Method -> m ResponseReceived,
    -- | what to do if the request path has no matches
    _rehUnmatchedPath :: MonadRespond m => m ResponseReceived,
    -- | what to do if components of the path can't be parsed
    _rehPathParseFailed :: MonadRespond m => [T.Text] -> m ResponseReceived,
    -- | what to do if the body failed to parse
    _rehBodyParseFailed :: MonadRespond m => ErrorReport -> m ResponseReceived,
    -- | what to do when authentication fails
    _rehAuthFailed :: MonadRespond m => ErrorReport -> m ResponseReceived,
    -- | what to do when authorization fails
    _rehDenied :: MonadRespond m => ErrorReport -> m ResponseReceived,
    -- | what to do when an exception has been caught
    _rehException :: MonadRespond m => ErrorReport -> m ResponseReceived
}

makeLenses ''RequestErrorHandlers

-- | this is the environment data used by RespondT. you probably don't want
-- to mess with this.
data RespondData = RespondData {
    _rehs :: RequestErrorHandlers,
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
    respond v = view responder >>= \r -> liftIO . r . toResponse $ v
    getRequest = view request
    getREHs = view rehs
    withREHs handlers = local (rehs %~ handlers)
    getPath = view pathConsumer
    withPath f = local (pathConsumer %~ f) 

-- | run the RespondT action with error handlers and request information.
runRespondT :: RespondT m a -> RequestErrorHandlers -> Request -> Responder -> m a
runRespondT (RespondT act) handlers req res = runReaderT act $ RespondData handlers req res (mkPathConsumer $ pathInfo req)

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
    newtype StT RespondT a = StRespond { unStRespond :: StT (ReaderT RespondData) a }
    liftWith = defaultLiftWith RespondT unRespondT StRespond
    restoreT = defaultRestoreT RespondT unStRespond

instance MonadBaseControl b m => MonadBaseControl b (RespondT m) where
    newtype StM (RespondT m) a = StMT { unStMT :: ComposeSt RespondT m a}
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM   unStMT

instance MonadLogger m => MonadLogger (RespondT m) where
    monadLoggerLog loc src level msg = lift $ monadLoggerLog loc src level msg

