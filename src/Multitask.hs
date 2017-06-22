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

import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as Ne
import System.Random

import Heap
import Stream

newtype Time = Time Double
  deriving (Show, Eq, Ord, Num, Fractional)

newtype Grade = Grade Double
  deriving (Show, Eq, Ord, Num, Fractional)

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

  atGrade :: job -> Grade -> (Time, job)
  jOld `atGrade` g = (ageOf jNew - ageOf jOld, jNew)
    where
      jNew = jOld `withGrade` g

  atTime :: NonEmpty job -> Time -> (Grade, NonEmpty job)
  jsOld `atTime` tTarget = go gOld (gOld + dgGuess) jsOld
    where
      -- TODO: figure out approximation subtleties.
      epsilon = Time 1e-16
      gOld = gradeOf (Ne.head jsOld)
      dgGuess = Grade 1.0
      go gLow gHigh jsPrev
        | abs (t - tTarget) < epsilon = (g, js)
        | t < tTarget = error "TODO: bisection"
        | t > tTarget = error "TODO: bisection"
        where
          g = (gLow + gHigh) / 2
          js = fmap (`withGrade` g) jsPrev
          t = sum (fmap ageOf js)

compareApprox epsilon x y
  | abs (x - y) < epsilon = EQ
  | otherwise = compare x y

bisect epsilon x0 dx0 f y =
  case compareApprox epsilon (f x0) y of
    LT -> searchUp x0 dx0
    EQ -> x0
    GT -> searchDown x0 dx0
  where
    searchUp x dx = error "TODO"
    searchDown x dx = error "TODO"
    refine xLow xHigh = error "TODO"

data Event = EvEnter | EvExit

simulate :: IsJob job => Stream (Timed job) -> Stream (Timed Event)
simulate arrs = unfold sim (Env 0 arrs Nothing [])

sim Env{..} = undefined


-- kv :: IsJob job =>
