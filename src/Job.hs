{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
#-}

module Job
  ( Time(..)
  , Grade(..)
  , IsJob(..)
  , JobOptimal(..)
  , JobSerptParallel(..)
  , JobSerptFirst(..)
  , JobBase(..)
  , randomJb
  , grade
  , totalAgeOf
  ) where

import Control.Lens ( Lens', lens )
import Control.Monad.State ( runState, state )
import Data.Foldable ( foldl' )
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as Ne
import Data.Semigroup.Foldable ( Foldable1 )
import System.Random

import Bisect
import Heap ( KeyVal(..) )

newtype Time = Time Double
  deriving (Show, Eq, Ord, Num, Fractional, Real, Random)

newtype Grade = Grade Double
  deriving (Show, Eq, Ord, Num, Fractional, Real)

class IsJob job where
  -- Create a job given number of tasks, starting age, and completion ages.
  -- Should use one of `jbPrepareFirst` or `jbPrepareParallel`.
  fromJb :: JobBase -> job
  jbOf :: job -> JobBase
  -- Measures time relative to some arbitrary base point, not actual age.
  -- The base point can change with each transition.
  ageOf :: job -> Time
  gradeOf :: job -> Grade
  withGrade :: job -> Grade -> job
  -- Gives state of the job right before and right after its next transition.
  -- Should use one of `jbTransitionFirst` or `jbTransitionParallel`.
  nextTransition :: job -> (job, Maybe job)
  -- Measures total amount of work left.
  workOf :: job -> Time
  workOf = workOfDefault
  ageAtGrade :: job -> Grade -> Time
  ageAtGrade = ageAtGradeDefault
  gradeAtTime ::
    (Foldable1 f, Functor f) =>
    KeyVal Grade (f job) -> Time -> Grade
  gradeAtTime = gradeAtTimeDefault

-- This assumes that ageOf uses total family age.
workOfDefault :: IsJob job => job -> Time
workOfDefault = (-) <$> sum . agesDone . jbOf <*> ageOf

ageAtGradeDefault :: IsJob job => job -> Grade -> Time
ageAtGradeDefault j g = ageOf (j `withGrade` g) - ageOf j

gradeAtTimeDefault ::
  (IsJob job, Foldable1 f, Functor f) =>
  KeyVal Grade (f job) -> Time -> Grade
gradeAtTimeDefault (Kv gOrig jsOrig) t =
  case bisect Bc{..} timeAtGrade t of
    -- We have no upper bound, so we shouldn't hit this case.
    BrTooHigh -> error "gradeAtTime: bisection failed. Maybe bad function?"
    -- If we pass a positive time, we shouldn't hit this case.
    BrTooLow -> error ("gradeAtTime: bisection failed. Maybe bad input? " ++ show t)
    BrJustRight gTarget -> gTarget
  where
    totalAgeOrig = totalAgeOf jsOrig
    timeAtGrade g = totalAgeOf (fmap (`withGrade` g) jsOrig) - totalAgeOrig
    xMin = gOrig
    xMax = positiveInfinity
    xGuess = xMin + 2*dxGuess
    dxGuess = 1.0
    dyEpsilon = 1e-9

grade :: IsJob job => Lens' job Grade
grade = lens gradeOf withGrade

totalAgeOf :: (IsJob job, Foldable f, Functor f) => f job -> Time
totalAgeOf = sum . fmap ageOf

randomJb :: RandomGen g => Int -> Time -> g -> (JobBase, g)
randomJb numTasks ageStart gen =
  flip runState gen $ do
    agesDone <- traverse genPareto (neReplicate numTasks ageStart)
    return Jb{..}
  where
    -- We rely on the fact that random for `Double` has a half-open range.
    genPareto age = paretoCdfInv age <$> state (randomR (0.0, 1.0))
    neReplicate n x = x :| replicate (n - 1) x

-- The different types are really different policies for the same jobs.
-- All jobs have no precedence constraints.
-- Within each job, all tasks are shifted Pareto of shape 2 and same scale.

data JobBase = Jb{
    ageStart :: Time
  , agesDone :: NonEmpty Time
  } deriving Show

-- Optimal policy: grade is fair reward, tasks served one at a time.
data JobOptimal = Jo{
    -- Must be at least `ageRest`.
    ageFirst :: Time
    -- Should be at least `ageFirst` while job is in system.
    -- But using `withGrade` in hypothetical computations are fine.
  , ageRest :: Time
    -- Must be `length (agesDone jbJo) - 1`.
  , numRest :: Int
  , jbJo :: JobBase
  } deriving Show

instance IsJob JobOptimal where

  fromJb jb@Jb{..} = Jo{
      ageFirst = ageStart
    , ageRest = ageStart
    , numRest = length agesDone - 1
    , jbJo = jbPrepareFirst jb
    }

  jbOf = jbJo

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
      jPre = j{ ageFirst = Ne.head agesDone }
      jqPost = do
        jbPost <- jbTransitionFirst jbJo
        return jPre{
            ageFirst = ageRest
          , numRest = numRest - 1
          , jbJo = jbPost
          }
      Jb{..} = jbJo

-- SERPT first: grade is expected size, tasks served one by one.
newtype JobSerptFirst = Jsf { joJsf :: JobOptimal }
  deriving Show

instance IsJob JobSerptFirst where

  fromJb = Jsf . fromJb
  jbOf = jbOf . joJsf
  ageOf = ageOf . joJsf
  workOf = workOf . joJsf
  gradeAtTime = gradeAtTimeSerpt

  gradeOf (Jsf Jo{..}) = Grade $ xFirst + (fromIntegral numRest * xRest)
    where
      Time xFirst = ageFirst
      Time xRest = ageRest

  withGrade (Jsf{..}) (Grade s) =
    Jsf joJsf{ ageFirst = Time $ s - (fromIntegral numRest * xRest) }
    where
      Time xRest = ageRest
      Jo{..} = joJsf

  nextTransition Jsf{..} = (Jsf joPre, Jsf <$> joqPost)
    where
      (joPre, joqPost) = nextTransition joJsf

-- SERPT parallel: grade is expected size, tasks served in parallel.
data JobSerptParallel = Jsp{
    ageAll :: Time
  , numAll :: Int
  , jbJsp :: JobBase
  } deriving Show

instance IsJob JobSerptParallel where

  fromJb jb@Jb{..} = Jsp{
      ageAll = ageStart
    , numAll = length agesDone
    , jbJsp = jbPrepareParallel jb
    }

  jbOf = jbJsp
  ageOf Jsp{..} = fromIntegral numAll * ageAll
  gradeOf j = Grade s where Time s = ageOf j
  gradeAtTime = gradeAtTimeSerpt

  withGrade j@Jsp{..} (Grade s) = j{ ageAll = Time $ s / fromIntegral numAll }

  nextTransition j@Jsp{..} = (jPre, jqPost)
    where
      jPre = j{ ageAll = Ne.head agesDone }
      jqPost = do
        jbPost <- jbTransitionParallel jbJsp
        return jPre{
            numAll = numAll - 1
          , jbJsp = jbPost
          }
      Jb{..} = jbJsp

-- For SERPT, grade is age, so we can compute gradeAtTime exactly.
gradeAtTimeSerpt ::
  (IsJob job, Foldable1 f, Functor f) =>
  KeyVal Grade (f job) -> Time -> Grade
gradeAtTimeSerpt (Kv (Grade s) js) (Time t) =
  Grade $ (t / fromIntegral (length js)) + s

jbPrepareFirst :: JobBase -> JobBase
jbPrepareFirst = id

jbPrepareParallel :: JobBase -> JobBase
jbPrepareParallel jb@Jb{..} = jb{ agesDone = Ne.sort agesDone }

jbTransitionFirst jb@Jb{..} = do
  ages <- snd (Ne.uncons agesDone)
  return jb{ agesDone = ages }

jbTransitionParallel jb@Jb{..} = do
  ages <- snd (Ne.uncons agesDone)
  return jb{ agesDone = ages }

repeated :: Int -> (a -> a) -> a -> a
repeated n f x = foldl' (flip ($)) x (replicate n f)

paretoCdfInv :: Time -> Double -> Time
paretoCdfInv (Time x) cdf = Time $ x / sqrt (1 - cdf)

-- The profit function for shifted Pareto of shape 2 uses `magic`.
-- We compute fair reward using the inverse, `magicInv`.

magic :: Double -> Double
magic x = x - 2 + 1/x

magicInv :: Double -> Double
magicInv x = (1 + x/2) + sqrt ((1 + x/2)^2 - 1)
