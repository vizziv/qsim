{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , RecordWildCards
#-}

module Bisect
  ( BisectConfig(..)
  , BisectResult(..)
  , bisect
  , positiveInfinity
  , negativeInfinity
  ) where

compareApprox :: (Num a, Ord a) => a -> a -> a -> Ordering
compareApprox epsilon x y
  | abs (x - y) < epsilon = EQ
  | otherwise = compare x y

data BisectConfig a b = Bc{
    xMin :: a
  , xMax :: a
  , xGuess :: a
  , dxGuess :: a
  , dyEpsilon :: b
  } deriving Show

data BisectResult a = BrTooHigh | BrTooLow | BrJustRight a
  deriving (Show, Eq, Ord, Foldable, Functor, Traversable)

-- Needs a monotonically increasing function!
bisect ::
  (Fractional a, Num b, Ord a, Ord b) =>
  BisectConfig a b -> (a -> b) -> b -> BisectResult a
bisect Bc{..} f y
  | f xMax < y = BrTooHigh
  | f xMin > y = BrTooLow
  | otherwise = BrJustRight $
    case compareApprox dyEpsilon (f xGuess) y of
      LT -> searchUp xGuess dxGuess
      EQ -> xGuess
      GT -> searchDown xGuess dxGuess
  where
    searchUp x dx
      | x + dx > xMax = searchBetween x xMax
      | f (x + dx) > y = searchBetween x (x + dx)
      | otherwise = searchUp (x + dx) (2 * dx)
    searchDown x dx
      | x - dx < xMin = searchBetween xMin x
      | f (x - dx) < y = searchBetween (x - dx) x
      | otherwise = searchDown (x - dx) (2 * dx)
    searchBetween xLow xHigh =
      case compareApprox dyEpsilon (f x) y of
        LT -> searchBetween x xHigh
        EQ -> x
        GT -> searchBetween xLow x
      where
        x = (xLow + xHigh) / 2

-- For some reason, this is the best way to define infinity.
-- Restricted to Real to ensure positivity and negativity.

positiveInfinity :: (Fractional a, Real a) => a
positiveInfinity = 1/0

negativeInfinity :: (Fractional a, Real a) => a
negativeInfinity = -1/0
