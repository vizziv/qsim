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
import Data.Foldable ( for_ )
import Data.Traversable ( for )
import System.Random

import Heap
import Job
import Stream ( Stream )
import qualified Stream

data Timed a = Timed Time a
  deriving (Show, Eq, Ord)

data Frames job = Frames{
    -- Grade of a frame itself is the current grade of all of its jobs.
    -- Within a frame, jobs are keyed by the grade of their next transition.
    -- Foreground frame contains active jobs and has minimal current grade.
    _foreground :: KeyVal Grade (Heap Grade job)
  , _background :: Maybe (Heap Grade (Heap Grade job))
  } deriving Show
makeLenses ''Frames

data Env job = Env{
    _timeSinceEvent :: Time
  , _arrivals :: Stream (Timed job)
  , _framesq :: Maybe (Frames job)
  } deriving Show
makeLenses ''Env

fg :: Traversal' (Env job) (KeyVal Grade (Heap Grade job))
fg = framesq . _Just . foreground

bgq :: Traversal' (Env job) (Maybe (Heap Grade (Heap Grade job)))
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
  case minimum . Compose $
       [ Just (Kv tArr AcArrival)
       , (Kv ?? AcTransition) <$> tqTrans
       , (Kv ?? AcMerge) <$> tqMerge
       ] of
    Kv t AcArrival -> do
      Timed t j <- doArrival
      frameqFg <- doAdvance t
      case frameqFg of
        Nothing ->
          framesq ?= Frames (frameOf j) Nothing
        Just (Kv gFg _) | gradeOf j > gFg ->
          bgq %= insert (frameOf j)
        Just frameFg | otherwise -> do
          fg .= frameOf j
          bgq %= insert frameFg
    Kv t AcTransition -> do
      timeSinceEvent += t
      frameFg <- preuse fg
      for_ frameFg $ \(Kv gOld hOld) -> do
        let jqNew = findMin hOld ^. val . to transitioned
            hqNew = deleteMin hOld & _Just . traverse . grade .~ gOld
        when (null jqNew) doExit
        case hqNew of
          Nothing ->
            framesq .= ((Frames ?? Nothing) . frameOf <$> jqNew)
          Just hNew ->
            case jqNew of
              Nothing -> do
                fg . key .= findMin hNew ^. val . to gradeOfFuture
                fg . val .= hNew
              Just jNew -> do
                fg .= frameOf jNew
                bgq %= insert (Kv gOld hNew)
    Kv t AcMerge -> do
      timeSinceEvent += t
      frameqBgMin <- preuse (bgq . _Just . to findMin)
      for_ frameqBgMin $ \(Kv gBg hBg) -> do
        fg . key .= gBg
        fg . val . traverse . grade .= gBg
        fg . val %= merge hBg
        bgq %= (>>= deleteMin)

timeArrival :: IsJob job => Simulation job Time
timeArrival = do
  tEv <- use timeSinceEvent
  Timed tArr _ <- use (arrivals . Stream.head)
  return (tArr - tEv)

timeqTransition :: IsJob job => Simulation job (Maybe Time)
timeqTransition = do
  hqFg <- preuse (fg . val)
  for hqFg $ \hFg -> timeUntilGrade (findMin hFg ^. key)

timeqMerge :: IsJob job => Simulation job (Maybe Time)
timeqMerge = do
  gqBg <- preuse (bgq . _Just . to findMin . key)
  traverse timeUntilGrade gqBg

timeUntilGrade :: IsJob job => Grade -> Simulation job Time
timeUntilGrade g = do
  hqFg <- preuse (fg . val)
  let hFg = maybe (error "timeUntilGrade: no jobs in system") id hqFg
      tNow = totalAgeOf hFg
      tFuture = totalAgeOf (hFg & traverse . grade .~ g)
  return (tFuture - tNow)

doArrival :: IsJob job => Simulation job (Timed job)
doArrival = do
  timeSinceEvent .= 0
  arr@(Timed t _) <- use (arrivals . Stream.head)
  arrivals %= view Stream.tail
  tell [Timed t EvEnter]
  return arr

doExit :: IsJob job => Simulation job ()
doExit = do
  t <- use timeSinceEvent
  timeSinceEvent .= 0
  tell [Timed t EvExit]

doAdvance ::
  IsJob job =>
  Time ->
  Simulation job (Maybe (KeyVal Grade (Heap Grade job)))
doAdvance t = do
  frameqFg <- preuse fg
  return $ frameqFg <&> \frameFg@(Kv _ hFg) ->
    let gFg = gradeAtTime frameFg t
    in Kv gFg (hFg & traverse . grade .~ gFg)

frameOf :: IsJob job => job -> KeyVal Grade (Heap Grade job)
frameOf j = Kv (gradeOf j) (singleton (Kv (gradeOfFuture j) j))

gradeOfFuture :: IsJob job => job -> Grade
gradeOfFuture = gradeOf . fst . nextTransition

transitioned :: IsJob job => job -> Maybe job
transitioned = snd . nextTransition
