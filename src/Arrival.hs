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
    numTasksMean = 1/2 * sumOf (both . to fromIntegral) numTasksRange
    ageStartMean = 1/2 * sumOf both ageStartRange
    Time s = numTasksMean * ageStartMean
    Time sDmrl = 1/2 * sumOf both sizeDmrlRange
    expCdfInv cdf = Time $ - addRates s sDmrl * log (1 - cdf)
    decideDmrl = fmap (< sDmrl) . state $ randomR (0, s + sDmrl)
    addRates x y = 1/(1/x + 1/y)
    go = do
      t <- fmap expCdfInv . state $ randomR (0, 1)
      isDmrl <- decideDmrl
      keep <- state $ randomR (0, 1)
      j <- case isDmrl of
        False -> do
          numTasks <- state $ randomR numTasksRange
          ageStart <- state $ randomR ageStartRange
          fmap Right . state $ randomJb numTasks ageStart
        True ->
          fmap (Left . uncurry Jd sizeDmrlRange) . state $
          randomR sizeDmrlRange
      return [(Delayed t j, keep)]

withLoad ::
  IsJob job =>
  Stream (Delayed (Either JobDmrl JobBase), Double) ->
  (Double, Double) ->
  Stream (Delayed (Either JobDmrl job))
withLoad arrs (load, loadDmrl) = Stream.unfold go (0, arrs)
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
