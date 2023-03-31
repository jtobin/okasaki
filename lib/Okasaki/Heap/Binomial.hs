{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Okasaki.Heap.Binomial where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix hiding (cata, ana, hylo)
import Data.Foldable (foldl')
import Data.Functor.Base (NonEmptyF(..))
import Data.Functor.Foldable
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE
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

-- arbitrary recursion
--
-- NB can't figure out how to express what seems like a very trivial recursion
--    scheme
--
--    we want to fold the list top-down and then recurse on the *entire*
--    folded list, not the rest of it.  i guess this is the issue.
--
pet :: Ord a => Tree a -> Heap a -> Heap a
pet a (Heap as) = Heap (loop a as) where
  loop s ts = case ts of
    [] -> pure s
    (h : t)
      | ran s < ran h -> s : ts
      | otherwise     -> loop (lin s h) t

put :: Ord a => a -> Heap a -> Heap a
put a (Heap as) = pet a ((nod 0 a []) :| as)

poot :: Ord a => a -> Heap a -> Heap a
poot a = pet (nod 0 a [])

mer :: Ord a => Heap a -> Heap a -> Heap a
mer (Heap l) (Heap r) = Heap (apo lag (l, r)) where
  lag (p, q) = case (project p, project q) of
    (u, Nil) -> fmap Left u
    (Nil, u) -> fmap Left u
    (Cons h s, Cons i t)
      | ran h < ran i -> Cons h (Right (s, r))
      | ran i < ran h -> Cons i (Right (l, t))
      | otherwise     -> Cons (lin h i) (Right (s, t))
      -- NB "pat (lin h i) (Right (s, t))"
      --
      --    pat has not been properly applied in the above case, but possible
      --    things are merged correctly.  test that ranks are strictly
      --    increasing, etc.
