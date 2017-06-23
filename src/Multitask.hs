{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
#-}

module Multitask
  ( Timed(..)
  , simulate
  ) where

import System.Random

import Heap
import Job
import Stream

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

data Event = EvEnter | EvExit

simulate :: IsJob job => Stream (Timed job) -> Stream (Timed Event)
simulate arrs = unfold sim (Env 0 arrs Nothing [])

sim :: IsJob job => Env job -> (Timed Event, Env job)
sim Env{..} = undefined
