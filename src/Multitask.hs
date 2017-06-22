{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  , TemplateHaskell
  , TupleSections
#-}

module Multitask where

import Prelude hiding ( head, tail )
import Control.Applicative ( liftA2 )
import Control.Lens
import Control.Monad.State
import Data.Monoid ( (<>) )

newtype Time = Time Double
  deriving (Eq, Ord, Show, Num)

{-
  For now, all jobs are aged Pareto with alpha = 2.
  Here "age" is not system age but family age.
  We parametrize `_taskAgeComplete` to show in the type system
  that it's hidden from the scheduler, but really `a` is `Time`.
 -}
data Task a = Task{
    _taskAgeNow :: Time
  , _taskAgeComplete :: a
  } deriving Show
makeFields ''Task

newtype Job a = Job{
    _jobTasks :: [Task a]
  } deriving Show
makeFields ''Job

data Arrival = Arrival{
    _arrivalDelay :: Time
  , _arrivalJob :: Job Time
  } deriving Show
makeFields ''Arrival

data Stream a = Stream{
    _streamHead :: a
  , _streamTail :: Stream a
  } deriving Show
makeFields ''Stream

-- The `Grade` of a job is its fair reward, expected size, or other metric.
newtype Grade = Grade Double
  deriving (Eq, Ord, Show, Num)

data Event = Event{
    _eventDelay :: Time
  , _eventIsArrival :: Bool
  } deriving Show
makeFields ''Event

{-
  Invariants:
  * Running jobs all have same grade.
  * Waiting jobs stored in frames of ascending grade order.
 -}
data Env = Env{
    _envDelay :: Time
  , _envArrivals :: Stream Arrival
  , _envJobsRunning :: [Job Time]
  , _envJobsWaiting :: [(Grade, [Job Time])]
  } deriving Show
makeFields ''Env

data Milestone =
    TaskCompletes Int
  | FramesMerge
  | JobArrives
  deriving (Eq, Ord, Show)

simulate ::
  -- Gives the grade of a job.
  -- This encodes the job-level policy.
  (forall a. Lens' (Job a) Grade) ->
  -- Given a job, gives the state of the job at the next task completion.
  -- This encodes the task-level policy.
  (Job Time -> Job Time) ->
  Stream Arrival ->
  Stream Event
simulate grade completed arrs = evalState go (Env 0 arrs [] [])
  where
    go = do
      env <- get
      let delayArrival =
            env ^. arrivals . head . delay . to (, JobArrives)
          gradeqRunning =
            minimumOf (gradesRunning <> gradeqWaiting) env
      return (error "TODO")
    gradesRunning =
      (jobsRunning .> traversed <. to completed . grade) .
      withIndex . to (\(i,g) -> (g, TaskCompletes i))
    gradeqWaiting =
      jobsWaiting . _head . _1 . to (, FramesMerge)
