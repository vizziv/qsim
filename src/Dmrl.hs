{-# LANGUAGE TemplateHaskell #-}

module Dmrl
  ( JobDmrl(..)
  , sizeExpected
  , sizeActual
  , ageBy
  , gradeOf
  , randomJd
  , expCdfInv
  ) where

import Control.Lens
import Control.Monad.State
import Data.Monoid ( (<>) )
import System.Random

import Job ( Grade(..), Time(..) )

-- Exponentially distributed.

data JobDmrl = Jd{
    _sizeExpected :: Time
  , _sizeActual :: Time
  } deriving (Read, Show)
makeLenses ''JobDmrl

ageBy :: JobDmrl -> Time -> JobDmrl
ageBy jd t = jd & sizeActual -~ t

gradeOf :: JobDmrl -> Grade
gradeOf jd = Grade s
  where
    Time s = jd ^. sizeExpected

expCdfInv :: Time -> Double -> Time
expCdfInv (Time s) cdf = Time $ -s * log (1 - cdf)

randomJd :: (RandomGen g, MonadState g m) => Time -> m JobDmrl
randomJd s = Jd s . expCdfInv s <$> state (randomR (0.0, 1.0))
