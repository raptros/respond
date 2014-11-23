{-|
Description: simple utilities for working with HLists.

These are some tools for working with HLists; this is relevant when you are using path extractors.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Respond.HListUtils where

import Data.HList

-- | this type family represents functions that can take all of the
-- elements of an HList.
type family HListElim (ts :: [*]) (a :: *) :: *
type instance HListElim '[] a = a
type instance HListElim (t ': ts) a = t -> HListElim ts a

-- | empty HList (useful for keeping other modules from needing to add
-- several language extensions).
type HList0 = HList '[]

-- | HList with one element.
type HList1 a = HList '[a]

-- | uncurrys a function by applying it to the elements of the HList.
hListUncurry :: HListElim ts a -> HList ts -> a
hListUncurry f HNil = f
hListUncurry f (HCons x xs) = hListUncurry (f x) xs

-- | uncurrys the function by applying it to the HList, then returns the
-- result of that uncurrying as a single-element HList. useful for
-- transforming PathExtractors.
hListMapTo1 :: HListElim ts a -> HList ts -> HList1 a
hListMapTo1 f hl = hListUncurry f hl .*. HNil
