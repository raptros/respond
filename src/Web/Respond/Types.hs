{-|
Description: base types 

contains a couple basic types for Respond
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Web.Respond.Types where

import Network.Wai
import qualified Data.Text as T
import Control.Applicative ((<$>))

import Control.Monad.Trans.State
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence as S
import Data.Aeson (FromJSON, eitherDecode)
import Data.String (fromString)

import Control.Lens (makeLenses, snoc, (%=), uses)
import Safe (headMay, tailSafe)

-- * responding

-- | instances of ToResponse can be converted into responses
class ToResponse a where
    -- | this is how
    toResponse :: a -> Response

instance ToResponse Response where
    toResponse = id

-- | the type of the responder callback that is handed to a WAI
-- 'Network.Wai.Application'
type Responder = Response -> IO ResponseReceived

-- * working with the path.

-- | stores the path and how much of it has been consumed
data PathConsumer = PathConsumer {
    -- | the consumed part of the path.
    _pcConsumed :: S.Seq T.Text,
    -- | the unconsumed part
    _pcUnconsumed :: [T.Text]
} deriving (Eq, Show)

makeLenses ''PathConsumer

-- | build a path consumer starting with nothing consumed
mkPathConsumer :: [T.Text] -> PathConsumer
mkPathConsumer = PathConsumer S.empty 

-- | get the next path element
pcGetNext :: PathConsumer -> Maybe T.Text
pcGetNext = headMay . _pcUnconsumed

-- | move forward in the path
pcConsumeNext :: PathConsumer -> PathConsumer
pcConsumeNext = execState $ do
    next <- uses pcUnconsumed headMay
    pcConsumed %= maybe id (flip snoc) next
    pcUnconsumed %= tailSafe

-- * working with the body

-- | represent an error by turning it into a bytestring.
class ErrorRep e where
    errorText :: e -> T.Text

instance ErrorRep T.Text where
    errorText = id

instance ErrorRep String where
    errorText = fromString

-- | something that can be pulled from the body, restricted to an error
-- type
class ErrorRep e => FromBody e a | a -> e where
    fromBody :: ErrorRep e => LBS.ByteString -> Either e a

-- | newtype for parsing from a json body
newtype Json a = Json { getJson :: a }

instance FromJSON a => FromBody String (Json a) where
    fromBody = (Json <$>) . eitherDecode

