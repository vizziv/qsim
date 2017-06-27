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
  , take
  , mapMaybe
  ) where

import Prelude hiding ( head, tail, take )
import Control.Lens.TH

data Stream a = Cons { _head :: a, _tail :: (Stream a) }
  deriving (Show, Foldable, Functor, Traversable)
makeLenses ''Stream

unfold :: (b -> ([a], b)) -> b -> Stream a
unfold f sOld = foldr Cons (unfold f sNew) xs
  where
    (xs, sNew) = f sOld

take :: Int -> Stream a -> [a]
take 0 _ = []
take n (Cons x xs) = x : take (n-1) xs

mapMaybe :: (a -> Maybe b) -> Stream a -> Stream b
mapMaybe f (Cons x xs) =
  case f x of
    Nothing -> mapMaybe f xs
    Just y -> Cons y (mapMaybe f xs)
