{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}

module Okasaki.Heap.Leftist (
    HeapF(..)
  , Heap(..)

  , lef
  , one
  , put
  , pat
  , bot
  , cut
  , wyt

  , sor
  , set
  , ran
  , mer

  , oil
  , gas
  ) where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix hiding (cata, ana, hylo)
import Data.Functor.Foldable
import Data.Monoid
import Okasaki.Orphans ()
import Text.Show.Deriving

-- NB arguably better to use induction
--
-- exercise 3.1: prove right spine contains at most floor(log(n + 1))
--               elements
--
-- * observe that rightmost-weighted binary tree satisfying leftist
--   property is balanced
-- * observe that right spine length is maximized in balanced case
-- * observe that tree has depth floor(log(n + 1)) in balanced case.
-- * therefore, right spine has at most floor(log(n + 1)) elements.
--   (QED)

data HeapF a r =
    LeafF
  | NodeF !(Sum Int) !a r r
  deriving (Eq, Functor, Foldable, Traversable, Show)

$(deriveShow1 ''HeapF)
$(deriveEq1 ''HeapF)

type Slew a = Fix (HeapF a)

newtype Heap a = Heap (Slew a)
  deriving Show

lef :: Heap a
lef = Heap (Fix LeafF)

uno :: a -> Slew a
uno x = Fix (NodeF 1 x (Fix LeafF) (Fix LeafF))

one :: a -> Heap a
one x = Heap (uno x)

nar :: Slew a -> Sum Int
nar h = case project h of
  LeafF -> mempty
  NodeF r _ _ _ -> r

ran :: Heap a -> Sum Int
ran (Heap h) = nar h

mer :: Ord a => Heap a -> Heap a -> Heap a
mer l = sor . mix l

mix :: Ord a => Heap a -> Heap a -> Heap a
mix (Heap l) (Heap r) = Heap (apo lag (l, r)) where
  lag (a, b) = case (project a, project b) of
    (c, LeafF) -> fmap Left c
    (LeafF, d) -> fmap Left d
    (NodeF _ m c d, NodeF _ n e f)
      | m <= n    -> NodeF (nar d <> nar b) m (Left c) (Right (d, b))
      | otherwise -> NodeF (nar a <> nar f) n (Left e) (Right (a, f))

sor :: Heap a -> Heap a
sor (Heap h) = Heap (cata alg h) where
  alg = \case
    LeafF -> Fix LeafF
    NodeF _ m l r -> set m l r

set :: a -> Slew a -> Slew a -> Slew a
set m l r
  | nar l >= nar r = Fix (NodeF (1 <> nar r) m l r)
  | otherwise      = Fix (NodeF (1 <> nar l) m r l)

put :: Ord a => a -> Heap a -> Heap a
put x = mer (one x)

-- exercise 3.2: direct insert
pat :: Ord a => a -> Heap a -> Heap a
pat p (Heap t) = Heap (pet p t) where
  pet x h = case project h of
    LeafF -> uno x
    NodeF _ m a b ->
      let (u, l)
            | x <= m    = (x, m)
            | otherwise = (m, x)

      in  uncurry (set u) (pot l a b)

  pot :: Ord a => a -> Slew a -> Slew a -> (Slew a, Slew a)
  pot l a b = case (project a, project b) of
    (_, LeafF) -> (a, uno l)
    (LeafF, _) -> (b, uno l)
    (NodeF _ c _ _, NodeF _ d _ _)
      | c > d     -> (pet l a, b)
      | otherwise -> (a, pet l b)

bot :: Heap a -> Maybe a
bot (Heap h) = case project h of
  LeafF -> Nothing
  NodeF _ b _ _ -> Just b

cut :: Ord a => Heap a -> Heap a
cut (Heap h) = case project h of
  LeafF -> Heap h
  NodeF _ _ l r -> mer (Heap l) (Heap r)

-- exercise 3.3: hylo gas
data BinF a r =
    EmpF
  | SinF !a
  | BinF r r
  deriving Functor

gas :: Ord a => [a] -> Heap a
gas = hylo alg lag where
  lag s = case project s of
    Nil        -> EmpF
    Cons h []  -> SinF h
    Cons {}    ->
      let (l, r) = splitAt (length s `div` 2) s
      in  BinF l r

  alg = \case
    EmpF     -> lef
    SinF a   -> one a
    BinF l r -> mer l r

oil :: Ord a => [a] -> Heap a
oil = cata $ \case
  Nil      -> lef
  Cons h t -> put h t

wyt :: Heap a -> Int
wyt (Heap h) = getSum (cata alg h) where
  alg = \case
    LeafF -> mempty
    NodeF _ _ l r -> 1 <> l <> r

-- test

-- (2) 1
--   |     \
-- (1) 2   (1) 3
--   |  \    |    \
--   L   L (1) 4   L
--           |    \
--         (1) 5   L
--           |    \
--           L     L

test0 :: Heap Int
test0 = gas [1..5]

-- (1) 1
--   |   \
-- (1) 2  L
--   |   \
-- (1) 3  L
--   |   \
-- (1) 4  L
--   |   \
-- (1) 5  L
--   |   \
--   L    L

test1 :: Heap Int
test1 = oil [1..5]
