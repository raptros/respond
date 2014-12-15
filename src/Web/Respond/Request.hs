{-|
Description: helpers for matching requests

contains various matching utilities
-}
{-# LANGUAGE TupleSections #-}
module Web.Respond.Request where

import Network.Wai
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative ((<$>))

import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Media as Media
import Data.Maybe (fromMaybe)

import Web.Respond.Types
import Web.Respond.Monad
import Web.Respond.Response

-- * extracting the request body

-- | gets the body as a lazy ByteString using lazy IO (see 'lazyRequestBody')
getBodyLazy :: MonadRespond m => m LBS.ByteString
getBodyLazy = getRequest >>= liftIO . lazyRequestBody

-- | gets the body as a lazy ByteString using /strict/ IO (see 'strictRequestBody')
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

-- | if given an error report value , respond immediately using 
-- 'handleDenied'. otherwise, run the inner route.
authorize :: (ReportableError e, MonadRespond m) => Maybe e -> m ResponseReceived -> m ResponseReceived
authorize check inner = maybe inner handleAccessDenied check

-- | if the bool is true, run the inner. otherwise, handleDenied the
-- report.
authorizeBool :: (ReportableError e, MonadRespond m) => e -> Bool -> m ResponseReceived -> m ResponseReceived
authorizeBool report allowed inner
    | allowed = inner
    | otherwise = handleAccessDenied report

-- | authorize using an Either; if it's Left, fail using 'handleDenied' on
-- the contained ReportableError. if it's right, run the inner action using
-- the contained value,
authorizeE :: (ReportableError e, MonadRespond m) => Either e a -> (a -> m ResponseReceived) -> m ResponseReceived
authorizeE check inner = either handleAccessDenied inner check

-- * content negotiation

-- | selects action by accept header
routeAccept :: MonadRespond m 
            => m a -- ^ default action - do this if nothing matches
            -> [(Media.MediaType, m a)] -- ^ actions to perform for each accepted media type
            -> m a -- ^ chosen action
routeAccept def mapped = getAcceptHeader >>= fromMaybe def . Media.mapAcceptMedia mapped

-- | defends the inner routes by first checking the Accept header and
-- failing if it cannot accept any media type in the list
checkAccepts :: MonadRespond m => [Media.MediaType] -> m ResponseReceived -> m ResponseReceived
checkAccepts list action = getAcceptHeader >>= maybe handleUnacceptableResponse (const action) . Media.matchAccept list
