{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
#-}

module Multitask
  ( Time(..)
  , Grade(..)
  , Timed(..)
  , IsJob(..)
  , simulate
  ) where

import Data.Semigroup.Foldable ( Foldable1 )
import System.Random

import Bisect
import Heap
import Stream

newtype Time = Time Double
  deriving (Show, Eq, Ord, Num, Fractional, Real)

newtype Grade = Grade Double
  deriving (Show, Eq, Ord, Num, Fractional, Real)

data Timed a = Timed Time a
  deriving (Show, Eq, Ord)

data Env job = Env{
    delay :: Time
  , arrivals :: Stream (Timed job)
    -- Single "grade frame" of foreground jobs, namely those running now.
  , jobsFg :: Maybe (KeyVal Grade (Heap Grade job))
    -- List of "grade frames" of background jobs, namely those not running now.
    -- Sorted by grade, lowest first.
  , jobsBg :: [KeyVal Grade (Heap Grade job)]
  } deriving Show

class IsJob job where
  ageOf :: job -> Time
  gradeOf :: job -> Grade
  withGrade :: job -> Grade -> job
  -- Gives state of the job right before and right after its next transition.
  nextTransition :: job -> (job, Maybe job)

  ageAtGrade :: job -> Grade -> Time
  ageAtGrade j g = ageOf (j `withGrade` g) - ageOf j

  gradeAtTime ::
    (Functor f, Foldable1 f) =>
    KeyVal Grade (f job) -> Time -> Grade
  gradeAtTime (Kv gOrig jsOrig) t =
    case bisect bs timeAtGrade t of
      -- We have no upper bound, so we shouldn't hit this case.
      BrTooHigh -> error "gradeAtTime: bisection failed. Maybe bad function?"
      -- If we pass a positive time, we shouldn't hit this case.
      BrTooLow -> error "gradeAtTime: bisection failed. Maybe bad input?"
      BrJustRight gTarget -> gTarget
    where
      totalAgeOf js = sum (fmap ageOf js)
      totalAgeOrig = totalAgeOf jsOrig
      timeAtGrade g = totalAgeOf (fmap (`withGrade` g) jsOrig) - totalAgeOrig
      bs = Bs{..}
      xMin = gOrig
      xMax = positiveInfinity
      xGuess = xMin + (2 * dxGuess)
      dxGuess = 1.0
      dyEpsilon = 1e-12

data Event = EvEnter | EvExit

simulate :: IsJob job => Stream (Timed job) -> Stream (Timed Event)
simulate arrs = unfold sim (Env 0 arrs Nothing [])

sim :: IsJob job => Env job -> (Timed Event, Env job)
sim Env{..} = undefined
