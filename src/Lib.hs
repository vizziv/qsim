{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    ( someFunc
    ) where

import qualified Picker ( max )

someFunc :: IO ()
someFunc = print (Picker.max id [1, 2, 3, 2, 3, 0])
