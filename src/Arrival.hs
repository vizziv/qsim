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

import Dmrl
import Job
import Stream ( Stream )
import qualified Stream

data Delayed a = Delayed { _delay :: Time, _object :: a }
  deriving (Show, Eq, Ord)
makeLenses ''Delayed

data ArrivalConfig = Ac{
    seed :: Int
  , numTasks :: Int
  , ageStart :: Time
  , sizeDmrl :: Time
  } deriving Show

poisson :: ArrivalConfig -> Stream (Delayed (Either JobDmrl JobBase), Double)
poisson Ac{..} = Stream.unfold (runState go) (mkStdGen seed)
  where
    rand r = state $ randomR r
    Time s = fromIntegral numTasks * ageStart
    Time sDmrl = sizeDmrl
    arrivalRate = 1/s + 1/sDmrl
    decideDmrl = (< 1/sDmrl) <$> rand (0, arrivalRate)
    go = do
      t <- expCdfInv (Time $ 1 / arrivalRate) <$> rand (0, 1)
      keep <- rand (0, 1)
      isDmrl <- decideDmrl
      j <- case isDmrl of
        False ->
          Right <$> randomJb numTasks ageStart
        True ->
          Left <$> randomJd sizeDmrl
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
