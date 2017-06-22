{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}

module Heap
  ( Heap
  , KeyVal(..)
  , findMin
  , singleton
  , insert
  , merge
  , deleteMin
  ) where

import Data.Function ( on )


-- Pairing heaps.

data Heap k v = Hp (KeyVal k v) [Heap k v]
  deriving (Show, Foldable, Functor, Traversable)

findMin :: Heap k v -> KeyVal k v
findMin (Hp kv _) = kv

singleton :: KeyVal k v -> Heap k v
singleton kv = Hp kv []

insert :: Ord k => KeyVal k v -> Heap k v -> Heap k v
insert = merge . singleton

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

data KeyVal k v = Kv { kvKey :: k, kvVal :: v }
  deriving (Show, Foldable, Functor, Traversable)

instance Eq k => Eq (KeyVal k v) where
  (==) = (==) `on` kvKey

instance Ord k => Ord (KeyVal k v) where
  compare = compare `on` kvKey
