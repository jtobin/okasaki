{-# OPTIONS_GHC -Wall #-}

module BinomialHeap where

data Tree a = Node Int a [Tree a] deriving Show

type Heap a = [Tree a]

empty :: Heap a
empty = []

isEmpty :: Heap a -> Bool
isEmpty = null

link :: Ord a => Tree a -> Tree a -> Tree a
link t0@(Node r x0 c0) t1@(Node _ x1 c1)
  | x0 <= x1  = Node (succ r) x0 (t1 : c0)
  | otherwise = Node (succ r) x1 (t0 : c1)

rank :: Tree a -> Int
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ x _) = x

insertTree :: Ord a => Tree a -> Heap a -> Heap a
insertTree t [] = [t]
insertTree t ts@(tree:trees)
  | rank t < rank tree = t : ts
  | otherwise          = insertTree (link t tree) trees

insert :: Ord a => a -> Heap a -> Heap a
insert x = insertTree (Node 0 x [])

merge :: Ord a => Heap a -> Heap a -> Heap a
merge ts [] = ts
merge [] ts = ts
merge ts0@(tree0:trees0) ts1@(tree1:trees1)
  | rank tree0 < rank tree1 = tree0 : merge trees0 ts1
  | rank tree1 < rank tree0 = tree1 : merge ts0 trees1
  | otherwise = insertTree (link tree0 tree1) (merge trees0 trees1)

removeMinTree :: Ord a => Heap a -> Maybe (Tree a, Heap a)
removeMinTree []     = Nothing
removeMinTree [t]    = return (t, [])
removeMinTree (t:ts) = do
  (tree, trees) <- removeMinTree ts
  return $
    if   root t < root tree
    then (t, ts)
    else (tree, t:trees)

findMin :: Ord a => Heap a -> Maybe a
findMin ts = do
  (t, _) <- removeMinTree ts
  return $ root t

deleteMin :: Ord a => Heap a -> Heap a
deleteMin ts = case removeMinTree ts of
  Nothing                  -> []
  Just (Node _ _ ts0, ts1) -> merge (reverse ts0) ts1

fromList :: Ord a => [a] -> Heap a
fromList = foldr insert empty

-- exercise 3.5 (findMin without call to removeMinTree)
altFindMin :: Ord a => Heap a -> Maybe a
altFindMin []     = Nothing
altFindMin [t]    = return $ root t
altFindMin (Node _ e _:ts) = do
  alt <- altFindMin ts
  return $
    if   e < alt
    then e
    else alt

-- exercise 3.7 (O(1) min in ocaml functor style)
class Heaplike h where
  hEmpty     :: h a
  hIsEmpty   :: h a -> Bool
  hInsert    :: Ord a => a -> h a -> h a
  hFindMin   :: Ord a => h a -> Maybe a
  hDeleteMin :: Ord a => h a -> h a

  hFromList :: Ord a => [a] -> h a
  hFromList = foldr hInsert hEmpty

data ExplicitMinHeap h a = E | NE a (h a) deriving Show

instance Heaplike h => Heaplike (ExplicitMinHeap h) where
  hEmpty = E

  hIsEmpty E = True
  hIsEmpty _ = False

  hInsert e E = NE e (hInsert e hEmpty)
  hInsert e (NE m h) = NE (min e m) (hInsert e h)

  hFindMin E = Nothing
  hFindMin (NE m _) = Just m

  hDeleteMin E = hEmpty
  hDeleteMin (NE m0 h) =
    let smaller = hDeleteMin h
    in  case hFindMin smaller of
          Nothing -> NE m0 smaller
          Just m1 -> NE (min m0 m1) smaller

-- example
newtype BinomialHeap a = BinomialHeap { unwrap :: Heap a } deriving Show

instance Heaplike BinomialHeap where
  hEmpty   = BinomialHeap empty
  hIsEmpty = isEmpty . unwrap
  hInsert e (BinomialHeap h)  = BinomialHeap $ insert e h
  hFindMin (BinomialHeap h)   = findMin h
  hDeleteMin (BinomialHeap h) = BinomialHeap $ deleteMin h

altFromList :: Ord a => [a] -> ExplicitMinHeap BinomialHeap a
altFromList = hFromList

