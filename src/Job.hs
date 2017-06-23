{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
#-}

module Job
  ( Time
  , Grade
  , IsJob(..)
  , JobOptimal
  , randomJob
  ) where

import Control.Monad ( replicateM )
import Control.Monad.State ( runState, state )
import Data.Foldable ( foldl' )
import Data.Semigroup.Foldable ( Foldable1 )
import System.Random

import Bisect
import Heap ( KeyVal(..) )

newtype Time = Time Double
  deriving (Show, Eq, Ord, Num, Fractional, Real)

newtype Grade = Grade Double
  deriving (Show, Eq, Ord, Num, Fractional, Real)

class IsJob job where
  -- Create a job given number of tasks, starting age, and completion ages.
  -- Can assume the number of tasks is the number of completion ages.
  create :: Int -> Time -> (Time, [Time]) -> job
  -- Measures time relative to some arbitrary base point, not actual age.
  -- The base point can change with each transition.
  ageOf :: job -> Time
  gradeOf :: job -> Grade
  withGrade :: job -> Grade -> job
  -- Gives state of the job right before and right after its next transition.
  nextTransition :: job -> (job, Maybe job)
  ageAtGrade :: job -> Grade -> Time
  ageAtGrade = ageAtGradeDefault
  gradeAtTime ::
    (Functor f, Foldable1 f) =>
    KeyVal Grade (f job) -> Time -> Grade
  gradeAtTime = gradeAtTimeDefault

ageAtGradeDefault :: IsJob job => job -> Grade -> Time
ageAtGradeDefault j g = ageOf (j `withGrade` g) - ageOf j

gradeAtTimeDefault ::
  (Functor f, Foldable1 f, IsJob job) =>
  KeyVal Grade (f job) -> Time -> Grade
gradeAtTimeDefault (Kv gOrig jsOrig) t =
  case bisect Bs{..} timeAtGrade t of
    -- We have no upper bound, so we shouldn't hit this case.
    BrTooHigh -> error "gradeAtTime: bisection failed. Maybe bad function?"
    -- If we pass a positive time, we shouldn't hit this case.
    BrTooLow -> error "gradeAtTime: bisection failed. Maybe bad input?"
    BrJustRight gTarget -> gTarget
  where
    totalAgeOf js = sum (fmap ageOf js)
    totalAgeOrig = totalAgeOf jsOrig
    timeAtGrade g = totalAgeOf (fmap (`withGrade` g) jsOrig) - totalAgeOrig
    xMin = gOrig
    xMax = positiveInfinity
    xGuess = xMin + 2*dxGuess
    dxGuess = 1.0
    dyEpsilon = 1e-12

randomJob :: (IsJob job, RandomGen g) => g -> (job, g)
randomJob gen =
  flip runState gen $ do
    numTasks <- genNumTasks
    ageStart <- genAgeStart
    ageFirstDone <- genPareto ageStart
    agesRestDone <- replicateM (numTasks - 1) (genPareto ageStart)
    return $ create numTasks ageStart (ageFirstDone, agesRestDone)
  where
    genNumTasks = state (randomR (1, 7))
    genAgeStart = return (Time 1.0)
    -- We rely on the fact that random for `Double` has a half-open range.
    genPareto age = paretoCdfInv age <$> state (randomR (0.0, 1.0))

-- The different types are really different policies for the same jobs.
-- All jobs have no precedence constraints.
-- Within each job, all tasks are shifted Pareto of shape 2 and same scale.

-- Optimal policy: grade is fair reward, tasks served one at a time.
data JobOptimal = Jo{
    -- Must be at least `ageRest`.
    ageFirst :: Time
    -- Should be at least `ageFirst` while job is in system.
    -- But using `withGrade` in hypothetical computations are fine.
  , ageFirstDone :: Time
  , ageRest :: Time
    -- All must be at least `ageRest`.
  , agesRestDone :: [Time]
    -- Must be length of `agesRestDone`.
  , numRest :: Int
  } deriving Show

instance IsJob JobOptimal where
  create numTasks ageStart (ageFirstDone, agesRestDone) =
    Jo{ ageFirst = ageStart, ageRest = ageStart, numRest = numTasks - 1, .. }

  ageOf Jo{..} = ageFirst + (fromIntegral numRest * ageRest)

  gradeOf Jo{..} = Grade $ xRest/2 * repeated numRest magicInv (xFirst / xRest)
    where
      Time xFirst = ageFirst
      Time xRest = ageRest

  withGrade j@Jo{..} (Grade r) =
    j{ ageFirst = Time $ xRest * repeated numRest magic (2 * r / xRest) }
    where
      Time xRest = ageRest

  nextTransition j@Jo{..} = (jPre, jqPost)
    where
      jPre = j{ ageFirst = ageFirstDone }
      jqPost =
        case agesRestDone of
          [] -> Nothing
          (age : ages) ->
            Just jPre{
                ageFirst = ageRest
              , ageRest = age
              , agesRestDone = ages
              , numRest = numRest - 1
              }

repeated :: Int -> (a -> a) -> a -> a
repeated n f x = foldl' (flip ($)) x (replicate n f)

paretoCdfInv :: Time -> Double -> Time
paretoCdfInv (Time x) cdf = Time $ x * (1/(sqrt (1 - cdf)) - 1)

-- The profit function for shifted Pareto of shape 2 uses `magic`.
-- We compute fair reward using the inverse, `magicInv`.

magic :: Double -> Double
magic x = x - 2 + 1/x

magicInv :: Double -> Double
magicInv x = (1 + x/2) + sqrt ((1 + x/2)^2 - 1)
