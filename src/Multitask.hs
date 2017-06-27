{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TemplateHaskell #-}

module Multitask
  ( Timed(..)
  , simulate
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Compose ( Compose(..) )
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as Ne
import Data.Foldable ( for_, traverse_ )
import Data.Traversable ( for )
import System.Random

import Heap
import Job
import Stream ( Stream )
import qualified Stream

data Timed a = Timed { _delay :: Time, _object :: a }
  deriving (Show, Eq, Ord)
makeLenses ''Timed

newtype Future a = Future { _fromFuture :: a }
  deriving (Show, Eq, Ord)
makeLenses ''Future

data Frames job = Frames{
    -- Grade of a frame itself is the current grade of all of its jobs.
    -- Within a frame, jobs are keyed by the grade of their next transition.
    -- Foreground frame contains active jobs and has minimal current grade.
    _foreground :: KeyVal Grade (Heap (Future Grade) job)
  , _background :: Maybe (Heap Grade (Heap (Future Grade) job))
  } deriving Show
makeLenses ''Frames

data Env job = Env{
    _timeSinceEvent :: Time
  , _arrivals :: Stream (Timed job)
  , _framesq :: Maybe (Frames job)
  } deriving Show
makeLenses ''Env

fg :: Traversal' (Env job) (KeyVal Grade (Heap (Future Grade) job))
fg = framesq . _Just . foreground

bgq :: Traversal' (Env job) (Maybe (Heap Grade (Heap (Future Grade) job)))
bgq = framesq . _Just . background

data Action = AcArrival | AcTransition | AcMerge

data Event = EvEnter | EvExit

simulate :: IsJob job => Stream (Timed job) -> Stream (Timed Event)
simulate arrs =
  Stream.unfold (runState (execWriterT sim)) (Env 0 arrs Nothing)

type Simulation job a = WriterT [Timed Event] (State (Env job)) a

sim :: IsJob job => Simulation job ()
sim = do
  tArr <- timeArrival
  tqTrans <- timeqTransition
  tqMerge <- timeqMerge
  case view val . minimum . Compose $
       [ Just (Kv tArr AcArrival)
       , (Kv ?? AcTransition) <$> tqTrans
       , (Kv ?? AcMerge) <$> tqMerge
       ] of
    AcArrival -> do
      j <- arrive
      frameqFg <- preuse fg
      case frameqFg of
        Nothing ->
          framesq ?= Frames (frameOf j) Nothing
        Just (Kv gFg _) | gradeOf j > gFg ->
          bgq %= insert (frameOf j)
        Just frameFg | otherwise -> do
          fg .= frameOf j
          bgq %= insert frameFg
    AcTransition -> do
      preuse (fg . val . keyMin) >>= traverse_ serveUntilGrade
      frameqFg <- preuse fg
      for_ frameqFg $ \(Kv g hPre) -> do
        let jqPost = findMin hPre ^. val . to transitioned
            hqPost = deleteMin hPre
        when (null jqPost) depart
        case hqPost of
          Nothing ->
            framesq .= ((Frames ?? Nothing) . frameOf <$> jqPost)
          Just hPost ->
            case jqPost of
              Nothing -> do
                fg . key .= findMin hPost ^. val . to gradeFuture . fromFuture
                fg . val .= hPost
              Just jPost -> do
                fg .= frameOf jPost
                bgq %= insert (Kv g hPost)
    AcMerge -> do
      frameqBgMin <- preuse (bgq . _Just . to findMin)
      for_ frameqBgMin $ \(Kv g h) -> do
        serveUntilGrade (Future g)
        fg . val %= merge h
        bgq %= (>>= deleteMin)

timeArrival :: IsJob job => Simulation job Time
timeArrival = do
  tEv <- use timeSinceEvent
  tArr <- use (arrivals . Stream.head . delay)
  return (tArr - tEv)

timeqTransition :: IsJob job => Simulation job (Maybe Time)
timeqTransition =
  preuse (fg . val . keyMin) >>= traverse timeUntilGrade

timeqMerge :: IsJob job => Simulation job (Maybe Time)
timeqMerge =
  preuse (bgq . _Just . keyMin . to Future) >>= traverse timeUntilGrade

timeUntilGrade :: IsJob job => Future Grade -> Simulation job Time
timeUntilGrade (Future g) = do
  hq <- preuse (fg . val)
  let h = maybe (error "timeUntilGrade: no jobs in system") id hq
      tNow = totalAgeOf h
      tFuture = totalAgeOf (h & mapped . grade .~ g)
  return (tFuture - tNow)

arrive :: IsJob job => Simulation job job
arrive = do
  arr@(Timed t j) <- use (arrivals . Stream.head)
  serveUntilTime t
  arrivals %= view Stream.tail
  timeSinceEvent .= 0
  tell [Timed t EvEnter]
  return j

depart :: IsJob job => Simulation job ()
depart = do
  t <- use timeSinceEvent
  arrivals . Stream.head . delay -= t
  timeSinceEvent .= 0
  tell [Timed t EvExit]

serveUntilTime :: IsJob job => Time -> Simulation job ()
serveUntilTime t =
  (preuse fg >>=) . traverse_ $ \frame ->
    serveUntilGrade (Future $ gradeAtTime frame t)

serveUntilGrade :: IsJob job => Future Grade -> Simulation job ()
serveUntilGrade (Future g) = do
  tBefore <- totalAgeOf . Compose <$> preuse (fg . val)
  fg . key .= g
  fg . val . traverse . grade .= g
  tAfter <- totalAgeOf . Compose <$> preuse (fg . val)
  timeSinceEvent += tAfter - tBefore

frameOf :: IsJob job => job -> KeyVal Grade (Heap (Future Grade) job)
frameOf j = Kv (gradeOf j) (singleton (Kv (gradeFuture j) j))

gradeFuture :: IsJob job => job -> Future Grade
gradeFuture = Future . gradeOf . fst . nextTransition

transitioned :: IsJob job => job -> Maybe job
transitioned = snd . nextTransition

keyMin :: Getter (Heap k v) k
keyMin = to findMin . key
