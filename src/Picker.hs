{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Picker
  ( Picker
  , pick
  , maxBy
  , minBy
  , maxWeightedBy
  , minWeightedBy
  ) where

import Control.Lens ( (&), (%~), _1, mapped )
import Control.Monad.Writer ( Writer, runWriter, tell )
import Data.Function ( on )
import Data.Semigroup ( Semigroup, Max(..), Min(..), Option(..) )

-- A function that assigns a weight to each element in a collection.
type Picker a =
  forall f i. Traversable f =>
  (i -> a) -> f i -> f (i, Double)

pick :: Traversable f => Picker a -> f a -> f (a, Double)
pick p xs = p id xs

-- The idea is that `dir` is either `Min` or `Max`.
optWeightedBy :: (Eq c, Semigroup c) => (b -> c) -> (a -> (b, Double)) -> Picker a
optWeightedBy dir metricWeight item xs = ys
  where
    (ys, metricMax) = runWriter (traverse go xs)
    mw = metricWeight . item & mapped . _1 %~ Option . Just . dir
    go i = do
      let (metricMe, weightMe) = mw i
          weight = if metricMe == metricMax then weightMe else 0
      tell metricMe
      return (i, weight)

maxWeightedBy :: Ord b => (a -> (b, Double)) -> Picker a
maxWeightedBy = optWeightedBy Max

minWeightedBy :: Ord b => (a -> (b, Double)) -> Picker a
minWeightedBy = optWeightedBy Min

optBy :: (Eq c, Semigroup c) => (b -> c) -> (a -> b) -> Picker a
optBy con metric = optWeightedBy con ((,1) . metric)

maxBy :: Ord b => (a -> b) -> Picker a
maxBy = optBy Max

minBy :: Ord b => (a -> b) -> Picker a
minBy = optBy Min
