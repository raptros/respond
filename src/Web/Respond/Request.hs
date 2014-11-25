{-|
Description: helpers for matching requests

contains various matching utilities
-}
module Web.Respond.Request where

import Network.Wai
import Network.HTTP.Types.Header
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative ((<$>))
import qualified Data.ByteString as BS

import Control.Monad.IO.Class (liftIO)

import Web.Respond.Types
import Web.Respond.Monad
import Web.Respond.Response
import Data.Aeson

-- * extracting the request body

-- | gets the body as a lazy ByteString using lazy IO (see 'Network.Wai.lazyRequestBody')
getBodyLazy :: MonadRespond m => m LBS.ByteString
getBodyLazy = getRequest >>= liftIO . lazyRequestBody

-- | gets you a function of the body, using lazy IO
getBodyAsLazy :: MonadRespond m => (LBS.ByteString -> a) -> m a
getBodyAsLazy = (<$> getBodyLazy)

-- ** as JSON

-- | parse the body as json, defer decoding
parseJsonBody :: (FromJSON a, MonadRespond m) => m (Maybe a)
parseJsonBody = getBodyAsLazy decode

-- | parse the body as json, decode immediately
parseJsonBody' :: (FromJSON a, MonadRespond m) => m (Maybe a)
parseJsonBody' = getBodyAsLazy decode'

-- | same as 'parseJsonBody' except wih an error string
parseJsonBodyEither :: (FromJSON a, MonadRespond m) => m (Either String a)
parseJsonBodyEither = getBodyAsLazy eitherDecode

-- | same as 'parseJsonBody'' except wih an error string
parseJsonBodyEither' :: (FromJSON a, MonadRespond m) => m (Either String a)
parseJsonBodyEither' = getBodyAsLazy eitherDecode'

-- ** fancy extraction

-- | use a FromBody instance to parse the body.
extractFromBody :: (ReportableError e, FromBody e a, MonadRespond m) => m (Either e a)
extractFromBody = getBodyAsLazy fromBody

withRequiredBody :: (ReportableError e, FromBody e a, MonadRespond m) => (a -> m ResponseReceived) -> m ResponseReceived
withRequiredBody action = extractFromBody >>= either handleBodyParseFailure action

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

-- | this is a stupid little trick.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond yes no = cond >>= \a -> if a then yes else no 

-- * headers
findHeader :: MonadRespond m => HeaderName -> m (Maybe BS.ByteString)
findHeader header = (lookup header) . requestHeaders <$> getRequest
