{-# LANGUAGE ExplicitForAll, Rank2Types, TupleSections #-}

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

-- A function that assigns a weight to each item in a collection.
-- By parametricity, a `Picker` cannot change the items in the collection.
type Picker item weight =
  forall f i. Traversable f =>
  (i -> item) -> f i -> f (i, weight)

pick :: Traversable f => Picker a w -> f a -> f (a, w)
pick p xs = p id xs

-- The idea is that `dir` is either `Min` or `Max`.
optWeightedBy ::
  (Eq s, Semigroup s) =>
  (key -> s) -> weight -> (item -> (key, weight)) -> Picker item weight
optWeightedBy dir zero metricWeight item xs = ys
  where
    (ys, metricMax) = runWriter (traverse go xs)
    mw = metricWeight . item & mapped . _1 %~ Option . Just . dir
    go i = do
      let (metricMe, weightMe) = mw i
          weight = if metricMe == metricMax then weightMe else zero
      tell metricMe
      return (i, weight)

maxWeightedBy ::
  Ord key =>
  weight -> (item -> (key, weight)) -> Picker item weight
maxWeightedBy = optWeightedBy Max

minWeightedBy ::
  Ord key =>
  weight -> (item -> (key, weight)) -> Picker item weight
minWeightedBy = optWeightedBy Min

optBy ::
  (Eq s, Semigroup s, Num weight) =>
  (key -> s) -> (item -> key) -> Picker item weight
optBy con metric = optWeightedBy con 0 ((,1) . metric)

maxBy :: (Ord key, Num weight) => (item -> key) -> Picker item weight
maxBy = optBy Max

minBy :: (Ord key, Num weight) => (item -> key) -> Picker item weight
minBy = optBy Min
