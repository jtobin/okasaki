{-# OPTIONS_GHC -Wall #-}

module Stack where

import Prelude hiding (head, tail)

data Stack a = Nil | Cons a (Stack a) deriving (Eq, Show)

instance Functor Stack where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

isEmpty :: Stack a -> Bool
isEmpty Nil = True
isEmpty _   = False

empty :: Stack a
empty = Nil

cons :: a -> Stack a -> Stack a
cons = Cons

head :: Stack a -> Maybe a
head Nil = Nothing
head (Cons h _) = return h

tail :: Stack a -> Maybe (Stack a)
tail Nil = Nothing
tail (Cons _ t) = return t

fromList :: [a] -> Stack a
fromList = foldr Cons Nil

toList :: Stack a -> [a]
toList Nil = []
toList (Cons h t) = h : toList t

append :: Stack a -> Maybe (Stack a) -> Maybe (Stack a)
append xs ys
  | isEmpty xs = ys
  | otherwise  = do
      h   <- head xs
      txs <- tail xs
      t   <- append txs ys
      return (cons h t)

update :: Stack a -> Int -> a -> Maybe (Stack a)
update Nil _ _ = Nothing
update (Cons h t) j y
  | j < 0     = Nothing
  | j == 0    = return (Cons y t)
  | otherwise = do
      nt <- update t (pred j) y
      return (Cons h nt)

-- exercise 2.1
suffixes :: Stack a -> Stack (Stack a)
suffixes Nil = Nil
suffixes (Cons _ t) = Cons t (suffixes t)

test :: Stack Int
test = fromList [1..10]

