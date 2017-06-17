{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    ( someFunc
    ) where

import Picker

someFunc :: IO ()
someFunc = print (pick (maxBy id) [1, 2, 3, 2, 3, 0])
