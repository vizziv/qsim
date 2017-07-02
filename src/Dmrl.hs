{-# LANGUAGE TemplateHaskell #-}

module Dmrl
  ( JobDmrl(..)
  , sizeMin
  , sizeMax
  , sizeActual
  , ageBy
  , gradeOf
  ) where

import Control.Lens
import Data.Monoid ( (<>) )

import Job ( Grade(..), Time(..) )

data JobDmrl = Jd{
    _sizeMin :: Time
  , _sizeMax :: Time
  , _sizeActual :: Time
  } deriving Show
makeLenses ''JobDmrl

ageBy :: JobDmrl -> Time -> JobDmrl
ageBy jd t =
  jd
  & sizeMin %~ subtractNonnegative t
  & sizeMax -~ t
  & sizeActual -~ t
  where
    subtractNonnegative x y = max 0 (y - x)

gradeOf :: JobDmrl -> Grade
gradeOf jd = Grade g
  where
    Time g = sumOf (sizeMin <> sizeMax) jd / 2
