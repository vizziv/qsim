{-# LANGUAGE ExplicitForAll, Rank2Types, TupleSections #-}

module Picker
  ( Picker
  , max
  , min
  , maxWeighted
  , minWeighted )
  where

import Prelude hiding ( min, max )
import Control.Lens ( (&), (%~), _1, mapped )
import Control.Monad.Writer ( Writer, runWriter, tell )
import Data.Function ( on )
import Data.Semigroup ( Semigroup, Max(..), Min(..), Option(..) )

-- A function that assigns a weight to each item in a collection.
--  parametricity, a `Picker` cannot change the items in the collection.
type Picker item weight =
  forall f i. Traversable f =>
  (i -> item) -> f i -> f (i, weight)

-- Type signature glossary:
-- * k: key
-- * w: weight
-- * s: semigroup

-- `dir` should be `Max` or `Min`.
optWeighted :: (Eq s, Semigroup s) => (k -> s) -> w -> Picker (k, w) w
optWeighted dir zero item xs = ys
  where
    (ys, keyMax) = runWriter (traverse go xs)
    keyWeight = item & mapped . _1 %~ Option . Just . dir
    go i = do
      let (keyMe, weightMe) = keyWeight i
          weight = if keyMe == keyMax then weightMe else zero
      tell keyMe
      return (i, weight)

maxWeighted :: Ord k => w -> Picker (k, w) w
maxWeighted = optWeighted Max

minWeighted :: Ord k => w -> Picker (k, w) w
minWeighted = optWeighted Min

opt :: (Eq s, Semigroup s, Num w) => (k -> s) -> Picker k w
opt con item = optWeighted con 0 ((,1) . item)

max :: (Ord k, Num w) => Picker k w
max = opt Max

min :: (Ord k, Num w) => Picker k w
min = opt Min
