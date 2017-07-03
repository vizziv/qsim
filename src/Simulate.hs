{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  , TemplateHaskell
  , TupleSections
#-}

module Simulate
  ( Event(..)
  , JobType(..)
  , simulate
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Compose ( Compose(..) )
import Data.Foldable ( for_, traverse_ )

import Arrival
import Dmrl hiding ( gradeOf )
import qualified Dmrl ( gradeOf )
import Heap
import Job
import Stream ( Stream )
import qualified Stream

data JobType = JtMulti | JtDmrl
  deriving (Show, Eq, Ord)

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
  , _arrivals :: Stream (Delayed (Either JobDmrl job))
  , _framesq :: Maybe (Frames job)
  , _jdsq :: Maybe (Heap Grade JobDmrl)
  , _jtqActive :: Maybe JobType
  } deriving Show
makeLenses ''Env

fg :: Traversal' (Env job) (KeyVal Grade (Heap (Future Grade) job))
fg = framesq . _Just . foreground

bgq :: Traversal' (Env job) (Maybe (Heap Grade (Heap (Future Grade) job)))
bgq = framesq . _Just . background

data Action = AcArrival | AcTransition JobType | AcSwap JobType
  deriving (Show, Eq, Ord)

data Event =
    -- Work of entering job.
    EvEnter JobType Time
    --- Number remaining, total work remaining.
  | EvExit JobType Int Time
  deriving (Show, Eq, Ord)

simulate ::
  IsJob job =>
  Stream (Delayed (Either JobDmrl job)) ->
  Stream
  ( Either
    (String, Maybe (Frames job), Maybe (Heap Grade JobDmrl))
    (Delayed Event)
  )
simulate arrs =
  Stream.unfold (runState (execWriterT sim)) $
  Env 0 arrs Nothing Nothing Nothing

type Simulation job =
  WriterT
  [ Either
    (String, Maybe (Frames job), Maybe (Heap Grade JobDmrl))
    (Delayed Event)
  ] (State (Env job))

sim :: IsJob job => Simulation job ()
sim = do
  acq <- argqMin timeq acsAll
  case acq of
    Nothing -> error "sim: no action, somehow...."
    Just ac -> act ac

timeq :: IsJob job => Action -> Simulation job (Maybe Time)
timeq = \case
  AcArrival -> do
    tEv <- use timeSinceEvent
    tArr <- use (arrivals . Stream.head . delay)
    return . Just $ tArr - tEv
  AcTransition JtMulti ->
    ifActive JtMulti $
    preuse (fg . val . keyMin) >>= traverse timeUntilGrade
  AcTransition JtDmrl ->
    ifActive JtDmrl $
    preuse (jdsq . _Just . valMin . sizeActual)
  AcSwap JtMulti ->
    ifActive JtMulti $
    preuse (bgq . _Just . keyMin . to Future) >>= traverse timeUntilGrade
  AcSwap JtDmrl ->
    -- `JtMulti` is correct here: we swap from multitask jobs to DMRL jobs.
    ifActive JtMulti $
    preuse (jdsq . _Just . keyMin . to Future) >>= traverse timeUntilGrade

act :: IsJob job => Action -> Simulation job ()
act = \case
  AcArrival -> do
    debug "AcArrival"
    jArr <- arrive
    case jArr of
      Left jd ->
        jdsq %= insert (kvf Dmrl.gradeOf jd)
      Right j -> do
        frameqFg <- preuse fg
        case frameqFg of
          Nothing ->
            framesq ?= Frames (frameOf j) Nothing
          Just (Kv gFg _) | gradeOf j > gFg ->
            bgq %= insert (frameOf j)
          Just frameFg | otherwise -> do
            fg .= frameOf j
            bgq %= insert frameFg
    setJtqActive
  AcTransition JtMulti -> do
    debug "AcTransition JtMulti"
    preuse (fg . val . keyMin) >>= traverse_ serveUntilGrade
    frameqFg <- preuse fg
    case frameqFg of
      Nothing ->
        error "act, AcTransition JtMulti: no foreground jobs"
      Just (Kv g hPre) -> do
        let jqPost = findMin hPre ^. val . to transitioned
            hqPost = deleteMin hPre
        case hqPost of
          Nothing ->
            case jqPost of
              Nothing -> do
                frameqBg <- preuse (bgq . _Just . kvMin)
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
        when (null jqPost) (depart JtMulti)
  AcTransition JtDmrl -> do
    debug "AcTransition JtDmrl"
    preuse (jdsq . _Just . valMin . sizeActual) >>= traverse_ serveUntilTime
    jdsq %= (>>= deleteMin)
    depart JtDmrl
  AcSwap JtMulti -> do
    debug "AcSwap JtMulti"
    frameqBgMin <- preuse (bgq . _Just . kvMin)
    case frameqBgMin of
      Nothing ->
        error "act, AcSwap JtMulti: no background jobs"
      Just (Kv g h) -> do
        serveUntilGrade (Future g)
        fg . val %= merge h
        bgq %= (>>= deleteMin)
  AcSwap JtDmrl -> do
    debug "AcSwap JtJtDmrl"
    gq <- preuse (jdsq . _Just . keyMin)
    case gq of
      Nothing ->
        error "act, AcSwap JtDmrl: no DMRL jobs"
      Just g -> do
        serveUntilGrade (Future g)
        -- Manually set `jtqActive` to break the grade tie.
        jtqActive ?= JtDmrl

arrive :: IsJob job => Simulation job (Either JobDmrl job)
arrive = do
  Delayed tArr jArr <- use (arrivals . Stream.head)
  tEv <- use timeSinceEvent
  serveUntilTime (tArr - tEv)
  arrivals %= view Stream.tail
  timeSinceEvent .= 0
  tell [Right . Delayed tArr $ EvEnter (jtOf jArr) (wOf jArr)]
  return jArr
  where
    jtOf (Left _) = JtDmrl
    jtOf (Right _) = JtMulti
    wOf = either (view sizeActual) workOf

depart :: IsJob job => JobType -> Simulation job ()
depart jt = do
  t <- use timeSinceEvent
  arrivals . Stream.head . delay -= t
  timeSinceEvent .= 0
  n <- get <&> lengthOf (framesq . _Just . each)
  w <- get <&> sumOf (framesq . _Just . each . to workOf)
  tell [Right . Delayed t $ EvExit jt n w]
  setJtqActive

setJtqActive :: IsJob job => Simulation job ()
setJtqActive = do
  jtq <- argqMin (preuse . gradeJt) [JtMulti, JtDmrl]
  jtqActive .= jtq
  where
    gradeJt JtMulti = fg . key
    gradeJt JtDmrl = jdsq . _Just . keyMin

serveUntilTime :: IsJob job => Time -> Simulation job ()
serveUntilTime t = do
  jtq <- use jtqActive
  case jtq of
    Just JtMulti ->
      (preuse fg >>=) . traverse_ $ \frame ->
        serveUntilGrade (Future $ gradeAtTime frame t)
    _ -> do
      timeSinceEvent += t
      jdsq . _Just . kvMin %= kvAgeBy t
  where
    kvAgeBy t (Kv _ jd) = kvf Dmrl.gradeOf $ ageBy jd t

serveUntilGrade :: IsJob job => Future Grade -> Simulation job ()
serveUntilGrade (Future g) = do
  jtq <- use jtqActive
  when (jtq /= Just JtMulti) $ error "serveUntilGrade: multitask jobs inactive"
  tBefore <- totalAgeOf . Compose <$> preuse (fg . val)
  fg . key .= g
  fg . val . each . grade .= g
  tAfter <- totalAgeOf . Compose <$> preuse (fg . val)
  timeSinceEvent += tAfter - tBefore

ifActive ::
  IsJob job =>
  JobType ->
  Simulation job (Maybe a) ->
  Simulation job (Maybe a)
ifActive jt x = do
  jtq <- use jtqActive
  if Just jt == jtq then x else return Nothing

timeUntilGrade :: IsJob job => Future Grade -> Simulation job Time
timeUntilGrade (Future g) = do
  hq <- preuse (fg . val)
  let h = maybe (error "timeUntilGrade: no foreground jobs") id hq
      tNow = totalAgeOf h
      tFuture = totalAgeOf (h & each . grade .~ g)
  return (tFuture - tNow)

debug :: IsJob job => String -> Simulation job ()
debug msg = tell =<< (:[]) . Left <$> ((msg,,) <$> use framesq <*> use jdsq)

frameOf :: IsJob job => job -> KeyVal Grade (Heap (Future Grade) job)
frameOf j = Kv (gradeOf j) (singleton (kvf gradeFuture j))

gradeFuture :: IsJob job => job -> Future Grade
gradeFuture = Future . gradeOf . fst . nextTransition

transitioned :: IsJob job => job -> Maybe job
transitioned = snd . nextTransition

argqMin ::
  (Traversable t, Applicative f, Ord b) =>
  (a -> f (Maybe b)) -> t a -> f (Maybe a)
argqMin f xs =
  fmap (view val) . minimumq . Compose <$> traverse kv xs
  where
    kv x = fmap (Kv ?? x) <$> f x
    minimumq = minimumOf traverse

acsAll :: [Action]
acsAll =
  AcArrival : [ac jt | ac <- [AcTransition, AcSwap], jt <- [JtMulti, JtDmrl]]
