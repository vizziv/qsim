{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module Arrival
  ( ArrivalConfig(..)
  , Delayed(..)
  , delay
  , object
  , poisson
  , withLoads
  ) where

import Control.Lens
import Control.Monad.State
import Data.Monoid ( (<>) )
import System.Random

import Dmrl ( JobDmrl(..) )
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
  , sizeDmrlRange :: (Time, Time)
  } deriving Show

poisson :: ArrivalConfig -> Stream (Delayed (Either JobDmrl JobBase), Double)
poisson Ac{..} = Stream.unfold (runState go) (mkStdGen seed)
  where
    rand r = state $ randomR r
    numTasksMean = 1/2 * sumOf (both . to fromIntegral) numTasksRange
    ageStartMean = 1/2 * sumOf both ageStartRange
    Time s = numTasksMean * ageStartMean
    Time sDmrl = 1/2 * sumOf both sizeDmrlRange
    arrivalRate = 1/s + 1/sDmrl
    expCdfInv cdf = Time $ - log (1 - cdf) / arrivalRate
    decideDmrl = (< 1/sDmrl) <$> rand (0, arrivalRate)
    go = do
      t <- expCdfInv <$> rand (0, 1)
      isDmrl <- decideDmrl
      keep <- rand (0, 1)
      j <- case isDmrl of
        False -> do
          numTasks <- rand numTasksRange
          ageStart <- rand ageStartRange
          fmap Right . state $ randomJb numTasks ageStart
        True ->
          Left . uncurry Jd sizeDmrlRange <$> rand sizeDmrlRange
      return [(Delayed t j, keep)]

withLoads ::
  IsJob job =>
  Stream (Delayed (Either JobDmrl JobBase), Double) ->
  (Double, Double) ->
  Stream (Delayed (Either JobDmrl job))
withLoads arrs (load, loadDmrl) = Stream.unfold go (0, arrs)
  where
    go (t, Stream.Cons (arr, keep) arrs) =
      if keep < relevantLoad then
        ([arr & delay +~ t & object . _Right %~ fromJb], (0, arrs))
      else
        ([], (t + arr ^. delay, arrs))
      where
        relevantLoad =
          case arr ^. object of
            Left _ -> loadDmrl
            Right _ -> load
