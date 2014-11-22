{-# OPTIONS_GHC -Wall #-}

module RedBlackTree where

data Color = Red | Black deriving (Eq, Show)

data Tree a = Leaf | Node Color (Tree a) a (Tree a) deriving Show

empty :: Tree a
empty = Leaf

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member e0 (Node _ l e1 r) = case compare e0 e1 of
  LT -> member e0 l
  GT -> member e0 r
  EQ -> True

singleton :: a -> Tree a
singleton e = Node Red Leaf e Leaf

insert :: Ord a => a -> Tree a -> Tree a
insert e0 s = Node Black a x b where
  ins Leaf = singleton e0
  ins t@(Node c l e1 r) = case compare e0 e1 of
    LT -> balance c (ins l) e1 r
    GT -> balance c l e1 (ins r)
    EQ -> t

  Node _ a x b = ins s

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance Black (Node Red (Node Red a x b) y c) z d =
  Node Red (Node Black a x b) y (Node Black c z d)

balance Black (Node Red a x (Node Red b y c)) z d =
  Node Red (Node Black a x b) y (Node Black c z d)

balance Black a x (Node Red (Node Red b y c) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)

balance Black a x (Node Red b y (Node Red c z d)) =
  Node Red (Node Black a x b) y (Node Black c z d)

balance c l e r = Node c l e r

-- exercise 3.8 (prove max depth of node is 2*floor(log (n + 1)))

-- exercise 3.9 (fromOrdList)
fromOrdList :: Ord a => [a] -> Tree a
fromOrdList = foldr insert empty

-- exercise 3.10 (improve balance)

