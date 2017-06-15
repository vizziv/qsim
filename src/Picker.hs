{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Picker
  ( Picker
  , pick
  , maxBy
  , minBy
  ) where

import Data.Ord (Down(..))
import Data.Semigroup (Semigroup(..))
import Data.Semigroup.Foldable (Foldable1(..))

data MaxKey k v = MaxKey k v deriving Show

instance Ord k => Semigroup (MaxKey k v) where
  kv1@(MaxKey k1 _) <> kv2@(MaxKey k2 _) =
    case compare k1 k2 of
      LT -> kv2
      _ -> kv1

type Picker a = forall f i. Foldable1 f => (i -> a) -> f i -> i

pick :: Foldable1 f => Picker a -> f a -> a
pick p xs = p id xs

maxBy :: Ord b => (a -> b) -> Picker a
maxBy metric item xs = i
  where
    MaxKey _ i = foldMap1 (\i -> (MaxKey (metric (item i)) i)) xs

minBy :: Ord b => (a -> b) -> Picker a
minBy metric = maxBy (Down . metric)
