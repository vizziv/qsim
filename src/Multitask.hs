{-# LANGUAGE
  DataKinds,
  FlexibleInstances,
  FunctionalDependencies,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  RankNTypes,
  TemplateHaskell
#-}

module Multitask where

import Control.Lens
import Control.Monad.State

newtype Time = Time Double deriving (Eq, Ord, Show, Num)

-- For now, all jobs are aged Pareto with alpha = 2.
-- Here "age" is not system age but family age.
data Task = Task{
    _taskAgeNow :: Time
  , _taskAgeComplete :: Time
  } deriving Show
makeFields ''Task

newtype Job = Job{
    _jobTasks :: [Task]
  } deriving Show
makeFields ''Job

data Arrival = Arrival{
    _arrivalDelay :: Time
  , _arrivalJob :: Job
  } deriving Show
makeFields ''Arrival

-- The `Grade` of a job is its fair reward, expected size, or other metric.
newtype Grade = Grade Double deriving (Eq, Ord, Show, Num)

data Event = Event{
    _eventDelay :: Time
  , _eventIsArrival :: Bool
  } deriving Show
makeFields ''Event

data Env = Env{
    _envDelay :: Time
  , _envArrivals :: [Arrival]
  , _envJobsRunning :: [Job]
  , _envJobsWaiting :: [Job]
  } deriving Show

simulate :: Lens' Job Grade -> [Arrival] -> [Event]
simulate grade arrs = evalState (go grade) (Env 0 arrs [] [])

go :: Lens' Job Grade -> State Env [Event]
go grade = error "TODO"
