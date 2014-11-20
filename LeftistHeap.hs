{-# OPTIONS_GHC -Wall #-}

module LeftistHeap where

import Control.Arrow

data Heap a = Leaf | Node Int a (Heap a) (Heap a) deriving (Eq, Show)

empty :: Heap a
empty = Leaf

isEmpty :: Heap a -> Bool
isEmpty Leaf = True
isEmpty _    = False

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h Leaf = h
merge Leaf h = h
merge h0@(Node _ e0 l0 r0) h1@(Node _ e1 l1 r1)
  | e0 <= e1  = create e0 l0 (merge r0 h1)
  | otherwise = create e1 l1 (merge h0 r1)

rank :: Heap a -> Int
rank Leaf = 0
rank (Node r _ _ _) = r

create :: a -> Heap a -> Heap a -> Heap a
create e l r
  | rank l >= rank r = Node (succ (rank r)) e l r
  | otherwise        = Node (succ (rank l)) e r l

singleton :: Ord a => a -> Heap a
singleton e = Node 1 e Leaf Leaf

insert :: Ord a => a -> Heap a -> Heap a
insert e = merge (singleton e)

findMin :: Heap a -> Maybe a
findMin Leaf = Nothing
findMin (Node _ e _ _) = return e

deleteMin :: Ord a => Heap a -> Heap a
deleteMin Leaf = Leaf
deleteMin (Node _ _ l r) = merge l r

fromList :: Ord a => [a] -> Heap a
fromList = foldr insert empty

-- exercise 3.1 (prove right-spine contains at most floor(log(n + 1)) elements)
-- - binary tree; observe that rightmost-weighted binary tree obeying leftist
--   property is balanced.
-- - right spine length is maximized in balanced case.
-- - tree has depth floor(log(n + 1)) in balanced case.
-- - right spine has at most floor(log(n + 1)) elements.

-- exercise 3.2 (define insert directly rather than by merge)
altInsert :: Ord a => a -> Heap a -> Heap a
altInsert e  Leaf            = singleton e
altInsert e0 (Node _ e1 l r) = create upper l0 r0 where
  (l0, r0) = cascadeInsert (l, r)
  (upper, lower)
    | e0 <= e1  = (e0, e1)
    | otherwise = (e1, e0)

  cascadeInsert (h, Leaf) = (h, singleton lower)
  cascadeInsert (Leaf, h) = (h, singleton lower)
  cascadeInsert hs@(Node _ x _ _, Node _ y _ _)
    | x > y     = first  (altInsert lower) hs
    | otherwise = second (altInsert lower) hs

