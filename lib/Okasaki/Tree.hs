{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}

module Okasaki.Tree (
    TreeF(..)
  , Tree
  , lef
  , nod

  , spy

  , put
  , pat
  , rub

  , has
  , haz

  , gas
  ) where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix hiding (cata, ana, hylo)
import Data.Functor.Foldable
import Data.Monoid
import Okasaki.Orphans ()
import qualified Okasaki.Tree.CPS as CPS
import Text.Show.Deriving

data TreeF a r =
    LeafF
  | NodeF r !a r
  deriving (Functor, Show)

$(deriveShow1 ''TreeF)
$(deriveEq1 ''TreeF)

type Tree a = Fix (TreeF a)

lef :: Tree a
lef = Fix LeafF

nod :: a -> Tree a -> Tree a -> Tree a
nod x l r = Fix (NodeF l x r)

put :: Ord a => a -> Tree a -> Tree a
put x = apo lag where
  lag pin = case project pin of
    LeafF -> NodeF (Left lef) x (Left lef)
    NodeF l e r -> case compare x e of
      EQ -> NodeF (Left l) e (Left r)
      LT -> NodeF (Right l) e (Left r)
      GT -> NodeF (Left l) e (Right r)

--  NB would be interesting to benchmark these without optimizations to
--     figure out the speed/memory profiles compared to the standard
--     versions

spy :: Ord a => a -> Tree a -> Maybe a
spy x = getLast . cata alg where
  alg = \case
    LeafF -> mempty
    NodeF l e r
      | x < e     -> l
      | otherwise -> Last (Just e) <> r

-- exercise 2.2 (max d + 1 comparisons)
haz :: Ord a => a -> Tree a -> Bool
haz x t = case spy x t of
  Nothing -> False
  Just s  -> s == x

-- exercise 2.3 (no unnecessary copying)
pat :: Ord a => a -> Tree a -> Tree a
pat x t = tug id t where
  tug k s = case project s of
    LeafF -> k (nod x lef lef)
    NodeF l e r -> case compare x e of
      EQ -> t
      LT -> tug (\a -> k (nod e a r)) l
      GT -> tug (\a -> k (nod e l a)) r

-- exercise 2.4 (no unnecessary copying, max d + 1 comparisons)
rub :: Ord a => a -> Tree a -> Tree a
rub x t = tug Nothing id t where
  tug c k s = case project s of
    LeafF -> case c of
      Nothing -> k (nod x lef lef)
      Just a
        | a == x    -> t
        | otherwise -> k (nod x lef lef)

    NodeF l e r
      | x < e     -> tug c (\a -> k (nod e a r)) l
      | otherwise -> tug (pure e) (\a -> k (nod e l a)) r

-- exercise 2.5a (construct balanced binary trees of depth n)
dap :: Ord a => a -> Int -> Tree a
dap x n = ana lag (n, lef) where
  lag (j, t)
    | j <= 0 = LeafF
    | otherwise =
        let s = (pred j, t)
        in  NodeF s x s

-- exercise 2.5b (construct mostly-balanced binary trees of size n)
sap :: Ord a => a -> Int -> Tree a
sap x n = ana lag (n, lef) where
  lag (j, t)
    | j <= 0 = LeafF
    | odd j  =
        let s = (j `quot` 2, t)
        in  NodeF s x s
    | otherwise =
        let l = j `quot` 2
            r = pred (j `quot` 2)
        in  NodeF (l, t) x (r, t)

gas :: Ord a => [a] -> Tree a
gas = cata $ \case
  Nil      -> lef
  Cons h t -> put h t

dep :: Integral b => Tree a -> b
dep = getSum . cata alg where
  alg = \case
    LeafF       -> mempty
    NodeF l _ r -> 1 <> max l r

wyt :: Integral b => Tree a -> b
wyt = getSum . cata alg where
  alg = \case
    LeafF       -> mempty
    NodeF l _ r -> 1 <> l <> r

has :: Ord a => a -> Tree a -> Bool
has x = cata $ \case
  LeafF       -> False
  NodeF l e r -> case compare x e of
    EQ -> True
    LT -> l
    GT -> r

ver :: CPS.Tree a -> Tree a
ver = ana lag where
  lag (project -> CPS.TreeF c) = c LeafF NodeF

rev :: Tree a -> CPS.Tree a
rev = ana lag where
  lag pin = case project pin of
    LeafF       -> CPS.lefF
    NodeF l e r -> CPS.nodF e l r

test0 :: Num a => Tree a
test0 = nod 1 lef lef

-- 1
-- | \
-- L  L

test1 :: Num a => Tree a
test1 = nod 2 (nod 1 lef lef) (nod 3 lef lef)

-- 2
-- |   \
-- 1    3
-- | \  | \
-- L  L L  L

test2 :: (Ord a, Num a) => Tree a
test2 = put 0 test1

-- 2
-- |   \
-- 1    3
-- | \  | \
-- 0  L L  L
-- | \
-- L  L

test3 :: (Ord a, Num a) => Tree a
test3 = put 5 test1

-- 2
-- |   \
-- 1    3
-- | \  | \
-- L  L L  5
--         | \
--         L  L

test4 :: (Ord a, Num a) => Tree a
test4 = put 4 test3

-- 2
-- |   \
-- 1    3
-- | \  | \
-- L  L L  5
--         | \
--         4  L
--         | \
--         L  L

