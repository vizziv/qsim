module Distribution
  ( Pareto(..) )
  where

data Pareto = Pareto{
    shape :: Double
  , scale :: Double
  , delay :: Double }
