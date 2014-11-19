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

-- exercise 2.6 (finite map using tree)
newtype Bin k v = Bin (k, v) deriving Show

instance Eq k => Eq (Bin k v) where
  Bin (k0, _) == Bin (k1, _) = k0 == k1

instance Ord k => Ord (Bin k v) where
  compare (Bin (k0, _)) (Bin (k1, _)) = compare k0 k1

newtype FiniteMap k v = FiniteMap (Tree (Bin k v)) deriving (Eq, Show)

emptyMap :: FiniteMap k v
emptyMap = FiniteMap Leaf

bind :: Ord k => k -> v -> FiniteMap k v -> FiniteMap k v
bind k v (FiniteMap m) = FiniteMap (insert' (Bin (k, v)) m) where
  insert' x Leaf = Node Leaf x Leaf
  insert' x (Node l e r) = case compare x e of
    EQ -> Node l x r
    LT -> Node (insert' x l) e r
    GT -> Node l e (insert' x r)

lookup :: Ord k => k -> FiniteMap k v -> Maybe v
lookup k (FiniteMap m) = lookup' m where
  lookup' Leaf = Nothing
  lookup' (Node l (Bin (key, v)) r) = case compare k key of
    EQ -> Just v
    LT -> lookup' l
    GT -> lookup' r

testMap :: FiniteMap Char Int
testMap = bind 'b' 2 (bind 'a' 1 emptyMap)

