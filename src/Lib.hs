module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello, world!"

otherThing = someFunc
