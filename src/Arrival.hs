{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module Arrival
  ( ArrivalConfig(..)
  , Delayed(..)
  , delay
  , object
  , poisson
  , withLoad
  ) where

import Control.Lens
import Control.Monad.State
import System.Random

import Job
import Stream ( Stream )
import qualified Stream

data Delayed a = Delayed { _delay :: Time, _object :: a }
  deriving (Show, Eq, Ord)
makeLenses ''Delayed

data ArrivalConfig = Ac{
    seed :: Int
  , numTasksRange :: (Int, Int)
  , ageStartRange :: (Time, Time)
  } deriving Show

poisson :: ArrivalConfig -> Stream (Delayed JobBase, Double)
poisson Ac{..} = Stream.unfold (runState go) (mkStdGen seed)
  where
    numTasksMean = 1/2 * sumOf (both . to fromIntegral) numTasksRange
    ageStartMean = 1/2 * sumOf both ageStartRange
    sizeMean@(Time s) = numTasksMean * ageStartMean
    expCdfInv cdf = Time $ (-s) * log (1 - cdf)
    go = do
      numTasks <- state $ randomR numTasksRange
      ageStart <- state $ randomR ageStartRange
      jb <- state $ randomJb numTasks ageStart
      t <- fmap expCdfInv . state $ randomR (0, 1)
      keep <- state $ randomR (0, 1)
      return [(Delayed t jb, keep)]

withLoad ::
  IsJob job =>
  Stream (Delayed JobBase, Double) ->
  Double ->
  Stream (Delayed job)
withLoad arrs load = Stream.unfold go (0, arrs)
  where
    go (t, Stream.Cons (arr, keep) arrs) =
      if keep < load then
        ([arr & delay +~ t & object %~ fromJb], (0, arrs))
      else
        ([], (t + arr ^. delay, arrs))
