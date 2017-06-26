{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , TemplateHaskell
#-}

module Stream
  ( Stream( Cons )
  , head
  , tail
  , unfold
  ) where

import Prelude hiding ( head, tail )
import Control.Lens.TH

data Stream a = Cons { _head :: a, _tail :: (Stream a) }
  deriving (Show, Foldable, Functor, Traversable)
makeLenses ''Stream

unfold :: (b -> ([a], b)) -> b -> Stream a
unfold f sOld = foldr Cons (unfold f sNew) xs
  where
    (xs, sNew) = f sOld

streamTake :: Int -> Stream a -> [a]
streamTake 0 _ = []
streamTake n (Cons x xs) = x : streamTake (n-1) xs
