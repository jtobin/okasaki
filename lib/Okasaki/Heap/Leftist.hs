{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}

module Okasaki.Heap.Leftist (
    HeapF(..)
  , Heap

  , lef
  , one
  , put
  , bot
  , cut

  , oil
  , gas
  ) where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix hiding (cata, ana, hylo)
import Data.Functor.Foldable
import Data.Monoid
import Okasaki.Orphans ()
import Text.Show.Deriving

data HeapF a r =
    LeafF
  | NodeF !Int !a r r
  deriving (Eq, Functor, Foldable, Traversable, Show)

$(deriveShow1 ''HeapF)
$(deriveEq1 ''HeapF)

type Heap a = Fix (HeapF a)

data BinF a r =
    EmpF
  | SinF !a
  | BinF r r
  deriving (Eq, Functor, Foldable, Traversable, Show)

type Bin a = Fix (BinF a)

lef :: Heap a
lef = Fix LeafF

one :: a -> Heap a
one x = Fix (NodeF 1 x lef lef)

ran :: Heap a -> Int
ran h = case project h of
  LeafF -> 0
  NodeF r _ _ _ -> r

mer :: Ord a => Heap a -> Heap a -> Heap a
mer l = sor . mix l

mix :: Ord a => Heap a -> Heap a -> Heap a
mix l r = apo lag (l, r) where
  lag (a, b) = case (project a, project b) of
    (c, LeafF) -> fmap Left c
    (LeafF, d) -> fmap Left d
    (NodeF _ m c d, NodeF _ n e f)
      | m <= n    -> NodeF 0 m (Left c) (Right (d, b))
      | otherwise -> NodeF 0 n (Left e) (Right (a, f))

sor :: Heap a -> Heap a
sor = cata $ \case
  LeafF -> lef
  NodeF _ m l r
    | ran l >= ran r -> Fix (NodeF (succ (ran r)) m l r)
    | otherwise      -> Fix (NodeF (succ (ran l)) m r l)

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

-- exercise 3.3: hylo gas
--
-- NB * characterise worst-case performance formally
--    * constructed heap differs from foldr'd put; confirm this is ok
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

spy :: Ord a => a -> Heap a -> Maybe a
spy x = getLast . cata alg where
  alg = \case
    LeafF -> mempty
    NodeF _ e l r
      | x < e     -> l
      | otherwise -> Last (Just e) <> r

haz :: Ord a => a -> Heap a -> Bool
haz x t = case spy x t of
  Nothing -> False
  Just s  -> s == x

-- reference

nodF :: a -> Heap a -> Heap a -> HeapF a (Heap a)
nodF x l r
  | ran l >= ran r = NodeF (succ (ran r)) x l r
  | otherwise      = NodeF (succ (ran l)) x r l

nod :: a -> Heap a -> Heap a -> Heap a
nod x l r = Fix (nodF x l r)

mux :: Ord a => Heap a -> Heap a -> Heap a
mux l r = case (project l, project r) of
  (_, LeafF) -> l
  (LeafF, _) -> r
  (NodeF _ m a b, NodeF _ n c d)
    | m <= n    -> nod m a (mux b r)
    | otherwise -> nod n c (mux l d)

oil :: Ord a => [a] -> Heap a
oil = cata $ \case
  Nil      -> lef
  Cons h t -> put h t

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
