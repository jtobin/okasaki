{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Okasaki.Tree.CPS (
    TreeF(..)
  , Tree
  , lef
  , lefF
  , nod
  , nodF

  , put
  , pet
  , has
  , haz
  , gas
  , wyt
  , dep

  , test0
  , test1
  , test2
  , test3
  , test4
  ) where

import Data.Fix hiding (cata, ana, hylo)
import Prelude hiding (sin)
import Data.Functor.Foldable
import Data.Monoid
import Okasaki.Orphans ()

newtype TreeF a r = TreeF (forall e. e -> (r -> a -> r -> e) -> e)
  deriving Functor

type Tree a = Fix (TreeF a)

lefF :: TreeF a r
lefF = TreeF const

nodF :: a -> r -> r -> TreeF a r
nodF x l r = TreeF (\_ c -> c l x r)

lef :: Tree a
lef = Fix lefF

nod :: a -> Tree a -> Tree a -> Tree a
nod x l r = Fix (nodF x l r)

sin :: a -> Tree a
sin x = nod x lef lef

empty :: Tree a -> Bool
empty (project -> TreeF c) = c True b where
  b _ _ _ = False

-- exercise 2.3 (no unnecessary copying) (?)
put :: Ord a => a -> Tree a -> Tree a
put x = apo lag where
  lag (project -> TreeF c) = c a b

  a = nodF x (Left lef) (Left lef)

  b l e r = case compare x e of
    EQ -> nodF e (Left l) (Left r)
    LT -> nodF e (Right l) (Left r)
    GT -> nodF e (Left l) (Right r)

has :: Ord a => a -> Tree a -> Bool
has x = cata alg where
  alg (TreeF c) = c False b

  b l e r = case compare x e of
    EQ -> True
    LT -> l
    GT -> r

-- exercise 2.2 (max d + 1 comparisons)
haz :: Ord a => a -> Tree a -> Bool
haz x t = case getLast (cata alg t) of
    Nothing -> False
    Just s  -> s == x
  where
    alg (TreeF c) = c mempty b

    b l e r
      | x < e     = l
      | otherwise = Last (Just e) <> r

-- exercise 2.4 (no unnecessary copying, max d + 1 comparisons)
pet :: Ord a => a -> Tree a -> Tree a
pet x t = apo lag (Nothing, t) where
  lag (s, project -> TreeF c) = c (a s) (b s)

  a = \case
    Nothing -> nodF x (Left lef) (Left lef)
    Just s
      | s == x    -> lefF
      | otherwise -> nodF x (Left lef) (Left lef)

  b s l e r
    | x < e       = nodF e (Right (s, l)) (Left r)
    | otherwise   = nodF e (Left l) (Right (Just e, r))

-- exercise 2.5a (construct balanced binary trees of depth n)
dap :: Ord a => a -> Int -> Tree a
dap x n = ana lag (n, lef) where
  lag (j, t)
    | j <= 0 = lefF
    | otherwise =
        let s = (pred j, t)
        in  nodF x s s
-- exercise 2.5b (construct mostly-balanced binary trees of size n)
sap :: Ord a => a -> Int -> Tree a
sap x n = ana lag (n, lef) where
  lag (j, t)
    | j <= 0 = lefF
    | odd j  =
        let s = (j `quot` 2, t)
        in  nodF x s s
    | otherwise =
        let l = j `quot` 2
            r = pred (j `quot` 2)
        in  nodF x (l, t) (r, t)

gas :: Ord a => [a] -> Tree a
gas = cata $ \case
  Nil      -> lef
  Cons h t -> put h t

dep :: Integral b => Tree a -> b
dep = getSum . cata alg where
  alg (TreeF c) = c mempty b
  b l _ r = Sum 1 <> max l r

wyt :: Integral b => Tree a -> b
wyt = getSum . cata alg where
  alg (TreeF c) = c mempty b
  b l _ r = Sum 1 <> l <> r

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
test2 = pet 0 test1

-- 2
-- |   \
-- 1    3
-- | \  | \
-- 0  L L  L
-- | \
-- L  L

test3 :: (Ord a, Num a) => Tree a
test3 = pet 5 test1

-- 2
-- |   \
-- 1    3
-- | \  | \
-- L  L L  5
--         | \
--         L  L

test4 :: (Ord a, Num a) => Tree a
test4 = pet 4 test3

-- 2
-- |   \
-- 1    3
-- | \  | \
-- L  L L  5
--         | \
--         4  L
--         | \
--         L  L

