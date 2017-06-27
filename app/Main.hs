module Main where

import Control.Lens
import Data.Monoid ( (<>) )
import Data.Foldable ( traverse_ )
import System.Environment

import Arrival
import Job
import Simulate
import Stream ( Stream )
import qualified Stream

{-
  Bug! Currently hangs on following runs (first one is exactly at bad point):
  ./.stack-work/install/x86_64-osx/lts-8.18/8.0.2/bin/qsim-exe 9003 0.8 11 11 1.0 3.0 94075
  ./.stack-work/install/x86_64-osx/lts-8.18/8.0.2/bin/qsim-exe 9002 0.9 1 8 1.0 20.0 100000
  ./.stack-work/install/x86_64-osx/lts-8.18/8.0.2/bin/qsim-exe 9001 0.9 1 3 1.0 100.0 10000
  Seems to be caused by large variability in starting age.
 -}

main :: IO ()
main = do
  [seed, _, numTasksLow, numTasksHigh, _, _, numEvents] <- map read <$> getArgs
  [_, load, _, _, ageStartLow, ageStartHigh, _] <- map read <$> getArgs
  run numEvents load Ac{
      seed = seed
    , numTasksRange = (numTasksLow, numTasksHigh)
    , ageStartRange = (Time ageStartLow, Time ageStartHigh)
    }

run :: Int -> Double -> ArrivalConfig -> IO ()
run numEvents load ac = do
  let nJo = meanNumInSystem numEvents $ simulate jos
      nJsf = meanNumInSystem numEvents $ simulate jsfs
      nJsp = meanNumInSystem numEvents $ simulate jsps
  putStrLn "Optimal, SERPT Series, SERPT Parallel (mean number in system)"
  traverse_ print [nJo, nJsf, nJsp]
  putStrLn "Optimal, SERPT Series, SERPT Parallel (normalized)"
  traverse_ (print . (/ nJo)) [nJo, nJsf, nJsp]
  where
    jbs = poisson ac
    jos = jbs `withLoad` load :: Stream (Delayed JobOptimal)
    jsfs = jbs `withLoad` load :: Stream (Delayed JobSerptFirst)
    jsps = jbs `withLoad` load :: Stream (Delayed JobSerptParallel)

ac :: ArrivalConfig
ac = Ac{ ageStartRange = (1.0, 2.0), numTasksRange = (7, 7), seed = 9001 }

jbs :: Stream (Delayed JobBase, Double)
jbs = poisson ac

jos :: Stream (Delayed JobOptimal)
jos = jbs `withLoad` 0.8

jsfs :: Stream (Delayed JobSerptFirst)
jsfs = jbs `withLoad` 0.8

jsps :: Stream (Delayed JobSerptParallel)
jsps = jbs `withLoad` 0.8

meanNumInSystem :: Int -> Stream (Either t (Delayed Event)) -> Double
meanNumInSystem n xs = f/d
  where
    (Time f, Time d, _) =
      foldl go (0, 0, 0) . Stream.take n . Stream.mapMaybe (preview _Right) $ xs
    go (f, d, n) (Delayed t ev) = (f + n*t, d + t, n + numOf ev)
    numOf EvEnter = 1
    numOf EvExit = (-1)

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
