module Main where

import Control.Lens
import Data.Monoid

import Arrival
import Job
import Simulate
import Stream ( Stream )

main :: IO ()
main = putStrLn "Load me in ghci."

ac :: ArrivalConfig
ac = Ac{ ageStartRange = (1.0, 1.0), numTasksRange = (1, 1), seed = 9001 }

jbs :: Stream (Delayed JobBase, Double)
jbs = poisson ac

jos :: Stream (Delayed JobOptimal)
jos = jbs `withLoad` 0.5

jsfs :: Stream (Delayed JobSerptFirst)
jsfs = jbs `withLoad` 0.5

jsps :: Stream (Delayed JobSerptParallel)
jsps = jbs `withLoad` 0.5

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
 -}

-- traverse_ (putStrLn . (++"\n") . show) . zip [0..] . Stream.take 100 $ simulate jsfs

stats xs = foldl go (0, 0, 0) xs
  where
    go (f, d, n) (Right (Delayed t ev)) = (f + n*t, d + t, n + numOf ev)
    go s _ = s
    numOf EvEnter = 1
    numOf EvExit = (-1)
