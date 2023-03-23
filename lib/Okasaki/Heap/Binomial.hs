{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}

module Okasaki.Heap.Binomial where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix hiding (cata, ana, hylo)
import Data.Functor.Foldable
import Text.Show.Deriving

data TreeF a r = NodeF Int a [Tree a]
  deriving (Functor, Foldable, Traversable)

type Tree a = Fix (TreeF a)

$(deriveShow1 ''TreeF)
$(deriveEq1 ''TreeF)

newtype Heap a = Heap [Tree a]

lef :: Heap a
lef = Heap []

nod :: Int -> a -> [Tree a] -> Tree a
nod r a t = Fix (NodeF r a t)

lin :: Ord a => Tree a -> Tree a -> Tree a
lin s@(project -> NodeF r a c) t@(project -> NodeF _ b d)
  | a <= b    = nod (succ r) a (t : c)
  | otherwise = nod (succ r) b (s : d)

ran :: Tree a -> Int
ran (project -> NodeF r _ _) = r

pat :: Ord a => Tree a -> Heap a -> Heap a
pat s (Heap ts) = Heap $ case project ts of
  Nil -> embed (Cons s (embed Nil))
  Cons h t
    | ran s < ran h -> embed (Cons s ts)
    | otherwise     -> embed (Cons (lin s h) t)

put :: Ord a => a -> Heap a -> Heap a
put a = pat (nod 0 a (embed Nil))
