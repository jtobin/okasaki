{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}

module Okasaki.Heap.Leftist.Weighted (
    HeapF(..)
  , Heap

  , lef
  , one
  , put
  , bot
  , cut

  , siz
  , wyt

  , oil
  , gas
  ) where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix hiding (cata, ana, hylo)
import Data.Functor.Foldable
import Data.Monoid
import Okasaki.Orphans ()
import Text.Show.Deriving

-- todo
--
-- * prove right-spine of weight-biased leftist heap contains at most
--   floor(log(n + 1)) elements

data HeapF a r =
    LeafF
  | NodeF !(Sum Int) !a r r
  deriving (Eq, Functor, Foldable, Traversable, Show)

$(deriveShow1 ''HeapF)
$(deriveEq1 ''HeapF)

type Heap a = Fix (HeapF a)

lef :: Heap a
lef = Fix LeafF

one :: a -> Heap a
one x = Fix (NodeF 1 x lef lef)

siz :: Heap a -> Sum Int
siz h = case project h of
  LeafF -> mempty
  NodeF r _ _ _ -> r

wyt :: Heap a -> Sum Int
wyt = cata $ \case
  LeafF         -> mempty
  NodeF _ _ l r -> 1 <> l <> r

mer :: Ord a => Heap a -> Heap a -> Heap a
mer l r = apo lag (l, r) where
  lag (a, b) = case (project a, project b) of
    (c, LeafF) -> fmap Left c
    (LeafF, d) -> fmap Left d
    (NodeF p m c d, NodeF q n e f)
      | m <= n && wyt c >= (wyt b <> wyt d) ->
          NodeF (p <> q) m (Left c) (Right (d, b))
      | m <= n ->
          NodeF (p <> q) m (Right (d, b)) (Left c)
      | m > n && wyt e >= (wyt a <> wyt f) ->
          NodeF (p <> q) n (Left e) (Right (a, f))
      | otherwise ->
          NodeF (p <> q) n (Right (a, f)) (Left e)

put :: Ord a => a -> Heap a -> Heap a
put x = mer (one x)

bot :: Heap a -> Maybe a
bot h = case project h of
  LeafF -> Nothing
  NodeF _ b _ _ -> Just b

cut :: Ord a => Heap a -> Heap a
cut h = case project h of
  LeafF -> h
  NodeF _ _ l r -> mer l r

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

-- test

-- (5) 1
--   |      \
-- (3) 3   (1) 2
--   |  \    | \
-- (2) 4 L   L  L
--   |  \
-- (1) 5 L
--   |  \
--   L   L

test0 :: Heap Int
test0 = gas [1..5]

-- (5) 1
--   |   \
-- (4) 2  L
--   |   \
-- (3) 3  L
--   |   \
-- (2) 4  L
--   |   \
-- (1) 5  L
--   |   \
--   L    L

test1 :: Heap Int
test1 = oil [1..5]
