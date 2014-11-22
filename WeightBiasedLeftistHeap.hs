{-# OPTIONS_GHC -Wall #-}

-- todo 3.4d, size of any subtree should be O(1)

module WeightBiasedLeftistHeap where

import Control.Arrow

-- exercise 3.4a (prove size is floor(log n + 1))
-- rationale is exactly equivalent to standard leftist heap

-- exercise 3.4b (rewrite leftist heap to use weight-biased property)
data Heap a = Leaf | Node a (Heap a) (Heap a) deriving (Eq, Show)

empty :: Heap a
empty = Leaf

isEmpty :: Heap a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Heap a -> Int
size Leaf         = 0
size (Node _ l r) = 1 + size l + size r

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h Leaf = h
merge Leaf h = h
merge h0@(Node e0 l0 r0) h1@(Node e1 l1 r1)
  | e0 <= e1  = create e0 l0 (merge r0 h1)
  | otherwise = create e1 l1 (merge h0 r1)

create :: a -> Heap a -> Heap a -> Heap a
create e l r
  | size l >= size r = Node e l r
  | otherwise        = Node e r l

singleton :: Ord a => a -> Heap a
singleton e = Node e Leaf Leaf

insert :: Ord a => a -> Heap a -> Heap a
insert e = merge (singleton e)

fromList :: Ord a => [a] -> Heap a
fromList = foldr insert empty

-- exercise 3.4c (make merge operate in a single top-down pass)
altMerge :: Ord a => Heap a -> Heap a -> Heap a
altMerge h Leaf = h
altMerge Leaf h = h
altMerge h0@(Node e0 l0 r0) h1@(Node e1 l1 r1) = Node e l r where
  (e, branches@(l2, r2), h)
    | e0 < e1   = (e0, (l0, r0), h1)
    | otherwise = (e1, (l1, r1), h0)

  (l, r)
    | size l2 <= size r2 + size h = first  (`altMerge` h) branches
    | otherwise                   = second (`altMerge` h) branches

-- exercise 3.4d (advantages of altMerge)
-- - lazy environment ?
-- - concurrent environment ?

