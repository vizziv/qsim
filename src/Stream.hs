{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
#-}

module Stream
  ( Stream(Cons)
  , unfold
  ) where

data Stream a = Cons a (Stream a)
  deriving (Show, Foldable, Functor, Traversable)

unfold :: (b -> (a, b)) -> b -> Stream a
unfold f sOld = let (x, sNew) = f sOld in Cons x (unfold f sNew)

streamTake :: Int -> Stream a -> [a]
streamTake 0 _ = []
streamTake n (Cons x xs) = x : streamTake (n-1) xs
