{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    ( someFunc
    ) where

import Picker
import Data.List.NonEmpty

someFunc :: IO ()
someFunc = print (pick (maxBy id) (1 :| [2, 3, 5, 67, 2, 53, 356]))
