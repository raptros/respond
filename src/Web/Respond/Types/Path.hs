{-|
Description: path consumer

tools for consuming request path
-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Respond.Types.Path where

import qualified Data.Text as T

import Control.Monad.Trans.State
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Monoid ((<>))

import Control.Lens (makeLenses, snoc, (%=), uses)
import Safe (headMay, tailSafe)

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

getFullPath :: PathConsumer -> [T.Text]
getFullPath pc = toList (_pcConsumed pc) <> _pcUnconsumed pc
