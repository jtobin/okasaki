{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}

module Okasaki.Heap.Binomial where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix hiding (cata, ana, hylo)
import Data.Foldable (toList)
import Data.Functor.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Text.Show.Deriving

data TreeF a r = NodeF !Int a [r]
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Tree a = Fix (TreeF a)

$(deriveShow1 ''TreeF)
$(deriveEq1 ''TreeF)

newtype Heap a = Heap [Tree a]
  deriving Show

lef :: Heap a
lef = Heap []

nod :: Int -> a -> [Tree a] -> Tree a
nod r a t = Fix (NodeF r a t)

lin :: Ord a => Tree a -> Tree a -> Tree a
lin s@(project -> NodeF r a c) t@(project -> NodeF q b d)
  | r /= q    = error "(okasaki, binomial): internal error"
  | a <= b    = nod (succ r) a (t : c)
  | otherwise = nod (succ r) b (s : d)

ran :: Tree a -> Int
ran (project -> NodeF r _ _) = r

roo :: Tree a -> a
roo (project -> NodeF _ a _) = a

pet :: Ord a => Tree a -> [Tree a] -> [Tree a]
pet a as = toList $ cata alg as (a :| as)
  where
    alg l n@(t :| ts) = case l of
      Nil -> n
      Cons h f
        | ran t < ran h -> n
        | otherwise     -> f $! lin t h :| drop 1 ts

put :: Ord a => a -> Heap a -> Heap a
put a (Heap as) = Heap $
  pet (nod 0 a []) as

mer :: Ord a => Heap a -> Heap a -> Heap a
mer (Heap a) (Heap b) = Heap $ mor a b where
  mor l r = case (l, r) of
    (p, []) -> p
    ([], q) -> q
    (h : t, i : s)
      | ran h < ran i -> h : mor t r
      | ran i < ran h -> i : mor l s
      | otherwise     -> pet (lin h i) (mor t s)

out :: Ord a => Heap a -> Maybe (Tree a, Heap a)
out (Heap as) = case as of
  []       -> Nothing
  (h : []) -> Just (h, lef)
  (h : t)  -> do
    (i, Heap s) <- out (Heap t)
    pure $ if   roo h <= roo i
           then (h, Heap t)
           else (i, Heap (h:s))

bot :: Ord a => Heap a -> Maybe a
bot h = do
  (a, _) <- out h
  pure $ roo a

-- exercise 3.5: direct bot
bed :: Heap a -> Maybe a
bed (Heap a) = case a of
  []      -> Nothing
  (h : _) -> Just (roo h)

cut :: Ord a => Heap a -> Heap a
cut h = fromMaybe lef $ do
  (project -> NodeF _ _ s, t) <- out h
  pure $ mer (Heap (reverse s)) t

-- reference

-- arbitrary recursion
pyt :: Ord a => Tree a -> Heap a -> Heap a
pyt a (Heap as) = Heap (loop a as) where
  loop s ts = case ts of
    [] -> pure s
    (h : t)
      | ran s < ran h -> s : ts
      | otherwise     -> loop (lin s h) t

-- test

test0 :: Heap Int
test0 = put 3 . put 2 . put 1 . put 0 $ lef

test1 :: Heap Int
test1 = put 4 test0
