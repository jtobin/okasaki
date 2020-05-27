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

blacken :: Tree a -> Tree a
blacken (Node _ l e r) = Node Black l e r
blacken Leaf = Leaf

insert :: Ord a => a -> Tree a -> Tree a
insert e0 = blacken . ins where
  ins Leaf = singleton e0
  ins t@(Node c l e1 r) = case compare e0 e1 of
    LT -> balance c (ins l) e1 r
    GT -> balance c l e1 (ins r)
    EQ -> t

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
-- - empty nodes black
-- - invariant 1: no red node has a red child
-- - invariant 2: every path from root to leaf contains same number of black nodes

-- exercise 3.9 (fromOrdList)
fromOrdList :: Ord a => [a] -> Tree a
fromOrdList = foldr insert empty

-- exercise 3.10a (improve balance)
lbalance :: Color -> Tree a -> a -> Tree a -> Tree a
lbalance Black (Node Red (Node Red a x b) y c) z d =
  Node Red (Node Black a x b) y (Node Black c z d)

lbalance Black (Node Red a x (Node Red b y c)) z d =
  Node Red (Node Black a x b) y (Node Black c z d)

lbalance c l e r = Node c l e r

rbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rbalance Black a x (Node Red (Node Red b y c) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)

rbalance Black a x (Node Red b y (Node Red c z d)) =
  Node Red (Node Black a x b) y (Node Black c z d)

rbalance c l e r = Node c l e r

altInsert :: Ord a => a -> Tree a -> Tree a
altInsert e0 = blacken . ins where
  ins Leaf = singleton e0
  ins t@(Node c l e1 r) = case compare e0 e1 of
    LT -> lbalance c (ins l) e1 r
    GT -> rbalance c l e1 (ins r)
    EQ -> t

altFromList :: Ord a => [a] -> Tree a
altFromList = foldr altInsert empty

-- exercise 3.10b (improve balance)
lrbalance :: Color -> Tree a -> a -> Tree a -> Tree a
lrbalance Black (Node Red a x (Node Red b y c)) z d =
  Node Red (Node Black a x b) y (Node Black c z d)

lrbalance c a x b = Node c a x b

llbalance :: Color -> Tree a -> a -> Tree a -> Tree a
llbalance Black (Node Red (Node Red a x b) y c) z d =
  Node Red (Node Black a x b) y (Node Black c z d)

llbalance c a x b = Node c a x b

rlbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rlbalance Black a x (Node Red (Node Red b y c) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)

rlbalance c a x b = Node c a x b

rrbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rrbalance Black a x (Node Red b y (Node Red c z d)) =
  Node Red (Node Black a x b) y (Node Black c z d)

rrbalance c a x b = Node c a x b

altAltInsert :: Ord a => a -> Tree a -> Tree a
altAltInsert e0 s = blacken $ ins s where
  ins Leaf = singleton e0
  ins (Node c l e1 r) = case compare e0 e1 of
    LT -> case l of
      Leaf -> Node c (ins l) e1 r
      Node _ _ e2 _
        | e0 <= e2  -> llbalance c (ins l) e1 r
        | otherwise -> lrbalance c (ins l) e1 r

    GT -> case r of
      Leaf -> Node c l e1 (ins r)
      Node _ _ e2 _
        | e0 <= e2  -> rlbalance c l e1 (ins r)
        | otherwise -> rrbalance c l e1 (ins r)

    EQ -> s

altAltFromList :: Ord a => [a] -> Tree a
altAltFromList = foldr altAltInsert empty

