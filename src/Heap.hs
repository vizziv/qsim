{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , DeriveAnyClass
  , FlexibleInstances
  , MultiParamTypeClasses
  , StandaloneDeriving
  , TemplateHaskell
#-}

module Heap
  ( Heap
  , KeyVal(..)
  , key
  , val
  , findMin
  , singleton
  , insert
  , merge
  , deleteMin
  , kvify
  ) where

import Control.Lens
import Control.Lens
import Data.Function ( on )
import Data.Semigroup.Foldable ( Foldable1 )

-- Pairing heaps.

data Heap k v = Hp (KeyVal k v) [Heap k v]
  deriving (Show, Foldable, Functor, Traversable)
deriving instance Foldable1 (Heap k)
instance Each (Heap k v1) (Heap k v2) v1 v2

findMin :: Heap k v -> KeyVal k v
findMin (Hp kv _) = kv

singleton :: KeyVal k v -> Heap k v
singleton kv = Hp kv []

insert :: Ord k => KeyVal k v -> Maybe (Heap k v) -> Maybe (Heap k v)
insert kv Nothing = Just $ singleton kv
insert kv (Just h) = Just $ merge (singleton kv) h

merge :: Ord k => Heap k v -> Heap k v -> Heap k v
merge h1@(Hp kv1 hs1) h2@(Hp kv2 hs2)
  | kv1 <= kv2 = Hp kv1 (h2 : hs1)
  | otherwise = Hp kv2 (h1 : hs2)

deleteMin :: Ord k => Heap k v -> Maybe (Heap k v)
deleteMin (Hp kv []) = Nothing
deleteMin (Hp kv hs) = Just (mergeAll hs)
  where
    -- We know the input is nonempty.
    mergeAll [] = error "impossible"
    mergeAll [h] = h
    mergeAll [h1, h2] = merge h1 h2
    mergeAll (h1 : h2 : hs) = merge (merge h1 h2) (mergeAll hs)

-- For storing values sorted by a key in a heap.
data KeyVal k v = Kv { _key :: k, _val :: v }
  deriving (Show, Foldable, Functor, Traversable, Foldable1)
makeLenses ''KeyVal

instance Eq k => Eq (KeyVal k v) where
  (==) = (==) `on` _key

instance Ord k => Ord (KeyVal k v) where
  compare = compare `on` _key

kvify :: Applicative f => (a -> f b) -> a -> f (KeyVal b a)
kvify f x = (Kv ?? x) <$> f x
