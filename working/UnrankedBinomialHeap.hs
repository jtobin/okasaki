{-# OPTIONS_GHC -Wall #-}

module UnrankedBinomialHeap where

-- exercise 3.6 (alternate-rank representation of binomial heaps)
data Tree a = Node a [Tree a] deriving Show

type Heap a = [(Int, Tree a)]

empty :: Heap a
empty = []

isEmpty :: Heap a -> Bool
isEmpty = null

link :: Ord a => (Int, Tree a) -> (Int, Tree a) -> (Int, Tree a)
link (r, t0@(Node x0 c0)) (_, t1@(Node x1 c1))
  | x0 <= x1  = (succ r, Node x0 (t1 : c0))
  | otherwise = (succ r, Node x1 (t0 : c1))

root :: Tree a -> a
root (Node x _) = x

insertTree :: Ord a => (Int, Tree a) -> Heap a -> Heap a
insertTree p [] = [p]
insertTree p0@(r0, _) ts@(p1@(r1, _):trees)
  | r0 < r1   = p0 : ts
  | otherwise = insertTree (link p0 p1) trees

insert :: Ord a => a -> Heap a -> Heap a
insert x = insertTree (0, Node x [])

merge :: Ord a => Heap a -> Heap a -> Heap a
merge ts [] = ts
merge [] ts = ts
merge ts0@(tree0:trees0) ts1@(tree1:trees1)
  | fst tree0 < fst tree1 = tree0 : merge trees0 ts1
  | fst tree1 < fst tree0 = tree1 : merge ts0 trees1
  | otherwise = insertTree (link tree0 tree1) (merge trees0 trees1)

removeMinTree :: Ord a => Heap a -> Maybe ((Int, Tree a), Heap a)
removeMinTree []  = Nothing
removeMinTree [p] = return (p, [])
removeMinTree (p0@(_, t):ts) = do
  (p1@(_, tree), trees) <- removeMinTree ts
  return $
    if   root t < root tree
    then (p0, ts)
    else (p1, p0:trees)

findMin :: Ord a => Heap a -> Maybe a
findMin ts = do
  ((_, t), _) <- removeMinTree ts
  return $ root t

deleteMin :: Ord a => Heap a -> Heap a
deleteMin ts = case removeMinTree ts of
  Nothing -> []
  Just ((r, Node _ ts0), ts1) ->
    let lesser = zip (repeat (pred r)) (reverse ts0)
    in  merge lesser ts1

fromList :: Ord a => [a] -> Heap a
fromList = foldr insert empty

