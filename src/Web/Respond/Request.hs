{-|
Description: helpers for matching requests

contains various matching utilities
-}
module Web.Respond.Request where

import Network.Wai
import Network.HTTP.Types.Method
import qualified Data.Map.Lazy as Map
import qualified Data.ByteString.Lazy as LBS
import Control.Lens (at, (^.), (<&>), to)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))

import Data.Monoid
import Data.Function (on)
import Control.Monad.IO.Class (liftIO)

import Web.Respond.Types
import Web.Respond.Monad
import Web.Respond.Response
import Data.Aeson

-- * matching methods
--
-- | Map from method to thing. use it as a monoid.
--
-- > onGET action1 <> onPUT action2
newtype MethodMatcher a = MethodMatcher { 
    getMethodMatcher :: Map.Map StdMethod a 
}

instance Monoid (MethodMatcher a) where
    mempty = MethodMatcher mempty
    mappend = (MethodMatcher .) . on mappend getMethodMatcher 

-- ** using the method matcher

-- | look up the request method in the MethodMatcher map and run the
-- action; fall back on 'handleUnsupportedMethod' if the method cannot be
-- parsed or is not in the map.
matchMethod :: MonadRespond m => MethodMatcher (m ResponseReceived) -> m ResponseReceived
matchMethod dispatcher = getRequest <&> (parseMethod . requestMethod) >>= either (handleUnsupportedMethod supported) selectMethod
    where
    supported = Map.keys (getMethodMatcher dispatcher)
    selectMethod mth = fromMaybe (handleUnsupportedMethod supported (renderStdMethod mth)) $ dispatcher ^. to getMethodMatcher . at mth

-- ** method matchers

-- | map a StdMethod to a thing.
onMethod :: StdMethod -> a -> MethodMatcher a
onMethod = (MethodMatcher .) . Map.singleton

onGET :: a -> MethodMatcher a
onGET = onMethod GET

onPOST :: a -> MethodMatcher a
onPOST = onMethod POST

onHEAD :: a -> MethodMatcher a
onHEAD = onMethod HEAD

onPUT :: a -> MethodMatcher a
onPUT = onMethod PUT

onDELETE :: a -> MethodMatcher a
onDELETE = onMethod DELETE

onTRACE :: a -> MethodMatcher a
onTRACE = onMethod TRACE

onCONNECT :: a -> MethodMatcher a
onCONNECT = onMethod CONNECT

onOPTIONS :: a -> MethodMatcher a
onOPTIONS = onMethod OPTIONS

onPATCH :: a -> MethodMatcher a
onPATCH = onMethod PATCH

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
authenticate :: MonadRespond m => m (Maybe a) -> (a -> m ResponseReceived) -> m ResponseReceived
authenticate auth inner = auth >>= maybe handleAuthFailed inner

-- | reauthenticate tries to use a prior authentication value to run the
-- inner action; if it's not availalble, it falls back to 'authenticate' to
-- apply the auth action and run the inner action.
reauthenticate :: MonadRespond m => Maybe a -> m (Maybe a) -> (a -> m ResponseReceived) -> m ResponseReceived
reauthenticate prior auth inner = maybe (authenticate auth inner) inner prior 

authorize :: MonadRespond m => m Bool -> m ResponseReceived -> m ResponseReceived
authorize check inner = ifM check inner handleDenied 

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond yes no = cond >>= \a -> if a then yes else no 
