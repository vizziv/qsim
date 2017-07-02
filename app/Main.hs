module Main where

import Control.Lens
import Data.Monoid ( (<>) )
import Data.Foldable ( traverse_ )
import System.Environment

import Dmrl ( JobDmrl )
import Arrival
import Job
import Simulate
import Stream ( Stream )
import qualified Stream

main :: IO ()
main = do
  [   seed
    , _
    , numTasksLow
    , numTasksHigh
    , _
    , _
    , _
    , _
    , _
    , numEvents
    ] <- map read <$> getArgs
  [   _
    , load
    , _
    , _
    , ageStartLow
    , ageStartHigh
    , loadDmrl
    , sizeDmrlLow
    , sizeDmrlHigh
    , _
    ] <- map read <$> getArgs
  run numEvents (load, loadDmrl) Ac{
      seed = seed
    , numTasksRange = (numTasksLow, numTasksHigh)
    , ageStartRange = (Time ageStartLow, Time ageStartHigh)
    , sizeDmrlRange = (Time sizeDmrlLow, Time sizeDmrlHigh)
    }

run :: Int -> (Double, Double) -> ArrivalConfig -> IO ()
run numEvents loads ac = do
  let nJo = stats numEvents $ simulate jos
      nJsf = stats numEvents $ simulate jsfs
      nJsp = stats numEvents $ simulate jsps
  putStrLn "Optimal, SERPT Series, SERPT Parallel (mean number in system)"
  putStrLn "Actually, reversed! (For finding the hanging bug.)"
  traverse_ print [nJsp, nJsf, nJo]
  putStrLn "Optimal, SERPT Series, SERPT Parallel (normalized)"
  traverse_ (print . (/ nJo)) [nJo, nJsf, nJsp]
  where
    (jos, jsfs, jsps) = streams ac loads

streams ::
  ArrivalConfig ->
  (Double, Double) ->
  ( Stream (Delayed (Either JobDmrl JobOptimal))
  , Stream (Delayed (Either JobDmrl JobSerptFirst))
  , Stream (Delayed (Either JobDmrl JobSerptParallel))
  )
streams ac =
  \loads -> (jbs `withLoad` loads, jbs `withLoad` loads, jbs `withLoad` loads)
  where
    jbs = poisson ac

statsWithErrs ::
  Int ->
  Stream (Either t (Delayed Event)) ->
  -- Mean number in system, discrepancies for number and work in system.
  (Double, [(Int, Time)])
statsWithErrs numSamples xs = (f/d, errs)
  where
    (Time f, Time d, _, _, errs) =
      foldl go (0, 0, 0, 0, []) .
      Stream.take numSamples .
      Stream.mapMaybe (preview _Right) $
      xs
    go (f, d, n, w, errs) (Delayed t ev) =
      ( f + (fromIntegral n * t)
      , d + t
      , nNew
      , wNew
      , errsOf ev ++ errs
      )
      where
        nNew = n + dnumOf ev
        wNew = (if n > 0 then w - t else w) + dworkOf ev
        dnumOf (EvEnter _ _) = 1
        dnumOf (EvExit _ _ _) = (-1)
        dworkOf (EvEnter _ w) = w
        dworkOf (EvExit _ _ _) = 0
        errsOf (EvEnter _ _) = []
        errsOf (EvExit _ nEv wEv) = [(nNew - nEv, wNew - wEv)]

-- Mean number in system.
-- A bit faster than `fst . statsWithErrs`.
stats :: Int -> Stream (Either t (Delayed Event)) -> Double
stats numSamples xs = f/d
  where
    (Time f, Time d, _) =
      foldl go (0, 0, 0) .
      Stream.take numSamples .
      Stream.mapMaybe (preview _Right) $
      xs
    go (f, d, n) (Delayed t ev) =
      ( f + (fromIntegral n * t)
      , d + t
      , n + dnumOf ev
      )
    dnumOf (EvEnter _ _) = 1
    dnumOf (EvExit _ _ _) = (-1)

sizeJb :: JobBase -> Time
sizeJb =
  sumOf $
  to agesDone . each <>
  to ((*) <$> negate . fromIntegral . length . agesDone <*> ageStart)

{-
  Check that load is really 0.5:

  >>> Stream.take 50000 jsfs &
  >>> (/) <$>
  >>> sumOf (each . object . to (sizeJb . jbJo . joJsf)) <*>
  >>> sumOf (each . delay)
  Time 0.5004608201008964

  For debugging.
  >>> traverse_ (putStrLn . (++"\n") . show) . zip [0..] . Stream.take 100 $ simulate jsfs

  This used to have a bug, but it works now.
  >>> last . Stream.take 6757 $ simulate jos
 -}

{-
  Past bug: used to hang on following runs (first one is exactly at bad point):
  ./.stack-work/install/x86_64-osx/lts-8.18/8.0.2/bin/qsim-exe 9003 0.8 11 11 1.0 3.0 94075
  ./.stack-work/install/x86_64-osx/lts-8.18/8.0.2/bin/qsim-exe 9002 0.9 1 8 1.0 20.0 100000
  ./.stack-work/install/x86_64-osx/lts-8.18/8.0.2/bin/qsim-exe 9001 0.9 1 3 1.0 100.0 10000
  ./.stack-work/install/x86_64-osx/lts-8.18/8.0.2/bin/qsim-exe 9001 0.8 1 1 1.0 1000.0 20
  Seems to be caused by large variability in starting age.
  Solution: increase acceptable bisection error.
 -}
