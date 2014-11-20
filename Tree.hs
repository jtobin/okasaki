{-# OPTIONS_GHC -Wall #-}

module Tree where

import Data.Maybe
import Prelude hiding (lookup)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

empty :: Tree a
empty = Leaf

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x t@(Node l e r) = case compare x e of
  EQ -> t
  LT -> Node (insert x l) e r
  GT -> Node l e (insert x r)

member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node l e r) = case compare x e of
  EQ -> True
  LT -> member x l
  GT -> member x r

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert empty

test :: Tree Int
test = fromList [5..10]

-- exercise 2.2 (max d + 1 comparisons)
altMember :: Ord a => a -> Tree a -> Bool
altMember x t = go t Nothing where
  go (Node l e r) acc
    | x < e     = go l acc
    | otherwise = go r (Just e)
  go Leaf Nothing  = False
  go Leaf (Just e) = e == x

-- exercise 2.3 (no unnecessary copying)
altInsert :: Ord a => a -> Tree a -> Tree a
altInsert x t = fromMaybe t (go t) where
  go Leaf = return (Node Leaf x Leaf)
  go (Node l e r) = case compare x e of
    EQ -> Nothing
    LT -> fmap (\alt -> Node alt e r) (go l)
    GT -> fmap (\alt -> Node l e alt) (go r)

-- exercise 2.4 (no unnecessary copying, max d + 1 comparisons)
efficientInsert :: Ord a => a -> Tree a -> Tree a
efficientInsert x t = fromMaybe t (go t Nothing) where
  go (Node l e r) acc
    | x < e     = fmap (\alt -> Node alt e r) (go l acc)
    | otherwise = fmap (\alt -> Node l e alt) (go r (Just e))
  go Leaf (Just e)
    | e == x    = Nothing
    | otherwise = go Leaf Nothing
  go Leaf Nothing = return (Node Leaf x Leaf)

-- exercise 2.5a (balanced binary trees of depth n)
completeDepth :: Ord a => a -> Int -> Tree a
completeDepth x n
  | n <= 0 = Leaf
  | otherwise =
      let t = completeDepth x (pred n)
      in  Node t x t

-- exercise 2.5b (mostly-balanced binary trees of size n)
completeSize :: Ord a => a -> Int -> Tree a
completeSize x n
  | n <= 0 = Leaf
  | odd n  =
      let t = completeSize x (n `quot` 2)
      in  Node t x t
  | otherwise =
      let l = completeSize x (n `quot` 2)
          r = completeSize x (n `quot` 2 - 1)
      in  Node l x r

