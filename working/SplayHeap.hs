{-# OPTIONS_GHC -Wall #-}

module SplayHeap where

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert x t = Node (smaller x t) x (bigger x t)

bigger :: Ord a => a -> Tree a -> Tree a
bigger _ Leaf = Leaf
bigger pivot (Node a x b)
  | x <= pivot = bigger pivot b
  | otherwise  = case a of
      Leaf       -> Node Leaf x b
      Node l y r ->
        if   y <= pivot
        then Node (bigger pivot r) x b
        else Node (bigger pivot l) y (Node r x b)

-- FIXME
smaller :: a
smaller = undefined

