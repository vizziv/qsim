{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TemplateHaskell
  , TupleSections
#-}

module Simulate
  ( Event(..)
  , simulate
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Compose ( Compose(..) )
import Data.Foldable ( for_, traverse_ )

import Arrival
import Dmrl ( JobDmrl )
import qualified Dmrl
import Heap
import Job
import Stream ( Stream )
import qualified Stream

newtype Future a = Future { _fromFuture :: a }
  deriving (Show, Eq, Ord)
makeLenses ''Future

data Frames job = Frames{
    -- Grade of a frame itself is the current grade of all of its jobs.
    -- Within a frame, jobs are keyed by the grade of their next transition.
    -- Foreground frame contains active jobs and has minimal current grade.
    _foreground :: KeyVal Grade (Heap (Future Grade) job)
  , _background :: Maybe (Heap Grade (Heap (Future Grade) job))
  } deriving (Show, Foldable, Functor, Traversable)
instance Each (Frames job) (Frames job) job job
makeLenses ''Frames

data Env job = Env{
    _timeSinceEvent :: Time
  , _arrivals :: Stream (Delayed job)
  , _framesq :: Maybe (Frames job)
  -- , _jdsq :: Maybe (Heap Grade JobDmrl)
  } deriving Show
makeLenses ''Env

fg :: Traversal' (Env job) (KeyVal Grade (Heap (Future Grade) job))
fg = framesq . _Just . foreground

bgq :: Traversal' (Env job) (Maybe (Heap Grade (Heap (Future Grade) job)))
bgq = framesq . _Just . background

data Action = AcArrival | AcTransition JobType | AcSwap JobType
  deriving (Show, Eq, Ord)

data JobType = JtMultitask | JtDmrl
  deriving (Show, Eq, Ord)

data Event =
    -- Work of entering job.
    EvEnter JobType Time
    --- Number remaining, total work remaining.
  | EvExit JobType Int Time
  deriving (Show, Eq, Ord)

simulate ::
  IsJob job =>
  Stream (Delayed job) ->
  Stream (Either (String, Maybe (Frames job)) (Delayed Event))
simulate arrs =
  Stream.unfold (runState (execWriterT sim)) (Env 0 arrs Nothing)

type Simulation job =
  WriterT [Either (String, Maybe (Frames job)) (Delayed Event)]
  (State (Env job))

sim :: IsJob job => Simulation job ()
sim = do
  kvqs <- traverse (getCompose . kvify (Compose . timeq)) $
         [ AcArrival
         , AcTransition JtMultitask
         , AcSwap JtMultitask
         ]
  case minimumOf (each . _Just) kvqs ^?! _Just . val of
    AcArrival -> do
      j <- arrive
      debug "AcArrival"
      frameqFg <- preuse fg
      case frameqFg of
        Nothing ->
          framesq ?= Frames (frameOf j) Nothing
        Just (Kv gFg _) | gradeOf j > gFg ->
          bgq %= insert (frameOf j)
        Just frameFg | otherwise -> do
          fg .= frameOf j
          bgq %= insert frameFg
    AcTransition JtMultitask -> do
      preuse (fg . val . keyMin) >>= traverse_ serveUntilGrade
      frameqFg <- preuse fg
      case frameqFg of
        Nothing ->
          error "sim, AcTransition JtMultitask: no foreground jobs"
        Just (Kv g hPre) -> do
          debug "AcTransition JtMultitask"
          let jqPost = findMin hPre ^. val . to transitioned
              hqPost = deleteMin hPre
          case hqPost of
            Nothing ->
              case jqPost of
                Nothing -> do
                  frameqBg <- preuse (bgq . _Just . to findMin)
                  case frameqBg of
                    Nothing -> do
                      framesq .= Nothing
                      debug "singleton fg, exits, no bg"
                    Just frameBg -> do
                      bgq %= (>>= deleteMin)
                      fg .= frameBg
                      debug "singleton fg, exits, some bg"
                Just jPost -> do
                  fg .= frameOf jPost
                  debug "singleton fg, stays"
            Just hPost ->
              case jqPost of
                Nothing -> do
                  fg . key .= findMin hPost ^. val . grade
                  fg . val .= hPost
                  debug "multi fg, exits"
                Just jPost -> do
                  fg .= frameOf jPost
                  bgq %= insert (Kv g hPost)
                  debug "multi fg, stays"
          when (null jqPost) depart
    AcSwap JtMultitask -> do
      frameqBgMin <- preuse (bgq . _Just . to findMin)
      case frameqBgMin of
        Nothing ->
          error "sim, AcSwap JtMultitask: no background jobs"
        Just (Kv g h) -> do
          serveUntilGrade (Future g)
          debug "AcSwap JtMultitask"
          fg . val %= merge h
          bgq %= (>>= deleteMin)

timeq :: IsJob job => Action -> Simulation job (Maybe Time)
timeq = \case
  AcArrival -> do
    tEv <- use timeSinceEvent
    tArr <- use (arrivals . Stream.head . delay)
    return . Just $ tArr - tEv
  AcTransition JtMultitask ->
    preuse (fg . val . keyMin) >>= traverse timeUntilGrade
  AcSwap JtMultitask ->
    preuse (bgq . _Just . keyMin . to Future) >>= traverse timeUntilGrade

timeUntilGrade :: IsJob job => Future Grade -> Simulation job Time
timeUntilGrade (Future g) = do
  hq <- preuse (fg . val)
  let h = maybe (error "timeUntilGrade: no foreground jobs") id hq
      tNow = totalAgeOf h
      tFuture = totalAgeOf (h & each . grade .~ g)
  return (tFuture - tNow)

arrive :: IsJob job => Simulation job job
arrive = do
  Delayed tArr j <- use (arrivals . Stream.head)
  tEv <- use timeSinceEvent
  serveUntilTime (tArr - tEv)
  arrivals %= view Stream.tail
  timeSinceEvent .= 0
  tell [Right . Delayed tArr . EvEnter JtMultitask . workOf $ j]
  return j

depart :: IsJob job => Simulation job ()
depart = do
  t <- use timeSinceEvent
  arrivals . Stream.head . delay -= t
  timeSinceEvent .= 0
  n <- get <&> lengthOf (framesq . _Just . each)
  w <- get <&> sumOf (framesq . _Just . each . to workOf)
  tell [Right . Delayed t $ EvExit JtMultitask n w]

serveUntilTime :: IsJob job => Time -> Simulation job ()
serveUntilTime t =
  (preuse fg >>=) . traverse_ $ \frame ->
    serveUntilGrade (Future $ gradeAtTime frame t)

serveUntilGrade :: IsJob job => Future Grade -> Simulation job ()
serveUntilGrade (Future g) = do
  tBefore <- totalAgeOf . Compose <$> preuse (fg . val)
  fg . key .= g
  fg . val . each . grade .= g
  tAfter <- totalAgeOf . Compose <$> preuse (fg . val)
  timeSinceEvent += tAfter - tBefore

debug :: IsJob job => String -> Simulation job ()
debug msg = return ()
-- tell =<< (:[]) . Left . (msg,) <$> use framesq

frameOf :: IsJob job => job -> KeyVal Grade (Heap (Future Grade) job)
frameOf j = Kv (gradeOf j) (singleton (Kv (gradeFuture j) j))

gradeFuture :: IsJob job => job -> Future Grade
gradeFuture = Future . gradeOf . fst . nextTransition

transitioned :: IsJob job => job -> Maybe job
transitioned = snd . nextTransition

keyMin :: Getter (Heap k v) k
keyMin = to findMin . key
