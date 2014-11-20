{-# OPTIONS_GHC -Wall #-}

module FiniteMap where

import Tree

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

