{-|
Description: helpers for matching requests

contains various matching utilities
-}
module Web.Respond.Request where

import Network.Wai
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative ((<$>))

import Control.Monad.IO.Class (liftIO)

import Web.Respond.Types
import Web.Respond.Monad
import Web.Respond.Response

-- * extracting the request body

-- | gets the body as a lazy ByteString using lazy IO (see 'Network.Wai.lazyRequestBody')
getBodyLazy :: MonadRespond m => m LBS.ByteString
getBodyLazy = getRequest >>= liftIO . lazyRequestBody

-- | gets the body as a lazy ByteString using /strict/ IO (see
-- 'Network.Wai.strictRequestBody'
getBodyStrict :: MonadRespond m => m LBS.ByteString
getBodyStrict = getRequest >>= liftIO . strictRequestBody

-- ** extraction using FromBody

-- | use a FromBody instance to parse the body. uses 'getBodyLazy' to
-- lazily load the body data.
extractBodyLazy :: (ReportableError e, FromBody e a, MonadRespond m) => m (Either e a)
extractBodyLazy = fromBody <$> getBodyLazy

-- | uses a FromBody instance to parse the body. uses 'getBodyStrict' to
-- load the body strictly.
extractBodyStrict :: (ReportableError e, FromBody e a, MonadRespond m) => m (Either e a)
extractBodyStrict = fromBody <$> getBodyStrict

-- | extracts the body using 'extractBodyLazy'. runs the inner action only
-- if the body could be loaded and parseda using the FromBody instance;
-- otherwise responds with the reportable error by calling
-- 'handleBodyParseFailure'.
withRequiredBody :: (ReportableError e, FromBody e a, MonadRespond m) => (a -> m ResponseReceived) -> m ResponseReceived
withRequiredBody action = extractBodyLazy >>= either handleBodyParseFailure action

-- | extracts the body using 'extractBodyStrict'. runs the inner action only
-- if the body could be loaded and parseda using the FromBody instance;
-- otherwise responds with the reportable error by calling
-- 'handleBodyParseFailure'.
withRequiredBody' :: (ReportableError e, FromBody e a, MonadRespond m) => (a -> m ResponseReceived) -> m ResponseReceived
withRequiredBody' action = extractBodyStrict >>= either handleBodyParseFailure action

-- * authentication and authorization

-- | authenticate uses the result of the authentication action (if it
-- succssfully produced a result) to run the inner action function.
-- otherwise, it uses 'handleAuthFailed'.
authenticate :: (MonadRespond m, ReportableError e) => m (Either e a) -> (a -> m ResponseReceived) -> m ResponseReceived
authenticate auth inner = auth >>= either handleAuthFailed inner

-- | reauthenticate tries to use a prior authentication value to run the
-- inner action; if it's not availalble, it falls back to 'authenticate' to
-- apply the auth action and run the inner action.
reauthenticate :: (MonadRespond m, ReportableError e) => Maybe a -> m (Either e a) -> (a -> m ResponseReceived) -> m ResponseReceived
reauthenticate prior auth inner = maybe (authenticate auth inner) inner prior 

-- | if the authorization action (m (Maybe e)) returns a value, respond
-- immediately using 'handleDenied'. if the auth action produces Nothing,
-- run the inner route.
authorize :: (ReportableError e, MonadRespond m) => m (Maybe e) -> m ResponseReceived -> m ResponseReceived
authorize check inner = check >>= maybe inner handleDenied

-- | authorize using an action that produces an Either. if the action
-- results in Left, fail using 'handleDenied' with the ReportableError.
-- if it results in Right, run the inner action using the produced value
authorizeE :: (ReportableError e, MonadRespond m) => m (Either e a) -> (a -> m ResponseReceived) -> m ResponseReceived
authorizeE check inner = check >>= either handleDenied inner
