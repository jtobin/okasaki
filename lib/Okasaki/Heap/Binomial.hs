{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}

module Okasaki.Heap.Binomial where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix hiding (cata, ana, hylo)
import Data.Foldable (foldl', toList)
import Data.Functor.Foldable
import Data.List.NonEmpty (NonEmpty(..))
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

-- top-down catamorphism
put :: Ord a => a -> Heap a -> Heap a
put a (Heap as) =
    let l = cata alg as (nod 0 a [] :| as)
    in  Heap (toList l)
  where
    alg l n@(t :| ts) = case l of
      Nil -> n
      Cons h f
        | ran t < ran h -> n
        | otherwise     -> f (lin t h :| drop 1 ts)

-- NB needs to be top-down
mer :: Ord a => Heap a -> Heap a -> Heap a
mer (Heap l) (Heap r) = Heap (apo lag (l, r)) where
  lag (p, q) = case (project p, project q) of
    (u, Nil) -> fmap Left u
    (Nil, u) -> fmap Left u
    (Cons h s, Cons i t)
      | ran h < ran i -> Cons h (Right (s, r))
      | ran i < ran h -> Cons i (Right (l, t))
      | otherwise     -> Cons (lin h i) (Right (s, t)) -- NB need 'put' call
      -- NB "pat (lin h i) (Right (s, t))"
      --
      --    pat has not been properly applied in the above case, but possible
      --    things are merged correctly.  test that ranks are strictly
      --    increasing, etc.

mar :: Ord a => Heap a -> Heap a -> Heap a
mar (Heap l) (Heap r) = case (l, r) of
  ([], r) -> r
  (l, []) -> l
  _       -> undefined

-- reference

pet :: Ord a => Tree a -> Heap a -> Heap a
pet a (Heap as) = Heap (loop a as) where
  loop s ts = case ts of
    [] -> pure s
    (h : t)
      | ran s < ran h -> s : ts
      | otherwise     -> loop (lin s h) t

pat :: Ord a => a -> Heap a -> Heap a
pat a (Heap as) =
    let l = foldl' alg (nod 0 a []) as
        r = dropWhile (\t -> ran t <= ran l) as
    in  Heap (l : r)
  where
    alg acc t
      | ran acc < ran t = acc
      | otherwise       = lin acc t

-- test

test0 :: Heap Int
test0 = put 3 . put 2 . put 1 . put 0 $ lef

test1 :: Heap Int
test1 = put 4 test0
