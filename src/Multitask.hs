{-# LANGUAGE
    GeneralizedNewtypeDeriving
#-}

module Multitask where

import Data.List.NonEmpty
import System.Random

import Heap
import Stream

newtype Time = Time Double
  deriving (Show, Eq, Ord, Num)

type Task = (Time, Time)

type Job = NonEmpty Task

data Env = Env{
    timeSinceLastEvent :: Time
  , arrivals :: Stream Job
  } deriving Show

x = arrivals
