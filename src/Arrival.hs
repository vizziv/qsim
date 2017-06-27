{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module Arrival
  ( ArrivalConfig(..)
  , Delayed(..)
  , delay
  , object
  , arrivalStream
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

data ArrivalConfig job = Ac{
    load :: Double
  , ageStartRange :: (Time, Time)
  , numTasksRange :: (Int, Int)
  , seed :: Int
  } deriving Show

arrivalStream :: IsJob job => ArrivalConfig job -> Stream (Delayed job)
arrivalStream Ac{..} = Stream.unfold (runState go) (mkStdGen seed, 0)
  where
    numTasksMean = 1/2 * sumOf (both . to fromIntegral) numTasksRange
    ageStartMean = 1/2 * sumOf both ageStartRange
    sizeMean@(Time s) = Time numTasksMean * ageStartMean
    expCdfInv cdf = Time $ (-s) * log (1 - cdf)
    go = do
      (arr, keep) <- zoom _1 $ do
        numTasks <- state $ randomR numTasksRange
        ageStart <- state $ randomR ageStartRange
        j <- state $ randomJob numTasks ageStart
        t <- fmap expCdfInv . state $ randomR (0, 1)
        keep <- fmap (< load) . state $ randomR (0, 1)
        return (Delayed t j, keep)
      case keep of
        True -> do
          t <- use _2
          _2 .= 0
          return [arr & delay +~ t]
        False -> do
          _2 += arr ^. delay
          return []
