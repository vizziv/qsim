module Main where

import Arrival
import Job
import Simulate
import Stream ( Stream )

main :: IO ()
main = putStrLn "Load me in ghci."

ac :: ArrivalConfig
ac = Ac{ ageStartRange = (1.0, 2.0), numTasksRange = (1, 2), seed = 9001 }

jos :: Stream (Delayed JobOptimal)
jos = poisson ac `withLoad` 0.5

jsfs :: Stream (Delayed JobSerptFirst)
jsfs = poisson ac `withLoad` 0.5

jsps :: Stream (Delayed JobSerptParallel)
jsps = poisson ac `withLoad` 0.5
