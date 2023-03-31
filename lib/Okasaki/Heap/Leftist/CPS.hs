{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}

module Okasaki.Heap.Leftist.CPS where

import Data.Fix hiding (cata, ana, hylo)
import Data.Functor.Foldable
import Data.Monoid
import Okasaki.Orphans ()

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

newtype HeapF a r = HeapF (forall e. e -> (Sum Int -> a -> r -> r -> e) -> e)
  deriving Functor

type Heap a = Fix (HeapF a)

lefF :: HeapF a r
lefF = HeapF const

lef :: Heap a
lef = Fix lefF

oneF :: a -> HeapF a (Heap b)
oneF x = HeapF (\_ c -> c 1 x lef lef)

one :: a -> Heap a
one = Fix . oneF

ran :: Heap a -> Sum Int
ran (project -> HeapF c) = c mempty b where
  b r _ _ _ = r

-- mer :: Ord a => Heap a -> Heap a -> Heap a
-- mer l = sor . mix l

-- mix :: Ord a => Heap a -> Heap a -> Heap a
-- mix l r = apo lag (l, r) where
--   lag (a, b) = case (project a, project b) of
--     (c, LeafF) -> fmap Left c
--     (LeafF, d) -> fmap Left d
--     (NodeF _ m c d, NodeF _ n e f)
--       | m <= n    -> NodeF (ran d <> ran b) m (Left c) (Right (d, b))
--       | otherwise -> NodeF (ran a <> ran f) n (Left e) (Right (a, f))

-- sor :: Heap a -> Heap a
-- sor = cata $ \case
--   LeafF -> lef
--   NodeF _ m l r -> set m l r
--
-- set :: a -> Heap a -> Heap a -> Heap a
-- set m l r
--   | ran l >= ran r = Fix (NodeF (1 <> ran r) m l r)
--   | otherwise      = Fix (NodeF (1 <> ran l) m r l)
--
-- put :: Ord a => a -> Heap a -> Heap a
-- put x = mer (one x)
--
-- -- exercise 3.2: direct insert
-- pat :: Ord a => a -> Heap a -> Heap a
-- pat x h = case project h of
--     LeafF -> one x
--     NodeF _ m a b ->
--       let (u, l)
--             | x <= m    = (x, m)
--             | otherwise = (m, x)
--
--       in  uncurry (set u) (pot l a b)
--   where
--     pot :: Ord a => a -> Heap a -> Heap a -> (Heap a, Heap a)
--     pot l a b = case (project a, project b) of
--       (_, LeafF) -> (a, one l)
--       (LeafF, _) -> (b, one l)
--       (NodeF _ c _ _, NodeF _ d _ _)
--         | c > d     -> (pat l a, b)
--         | otherwise -> (a, pat l b)
--
-- bot :: Heap a -> Maybe a
-- bot h = case project h of
--   LeafF -> Nothing
--   NodeF _ b _ _ -> Just b
--
-- cut :: Ord a => Heap a -> Heap a
-- cut h = case project h of
--   LeafF -> h
--   NodeF _ _ l r -> mer l r
--
-- -- exercise 3.3: hylo gas
-- data BinF a r =
--     EmpF
--   | SinF !a
--   | BinF r r
--   deriving Functor
--
-- gas :: Ord a => [a] -> Heap a
-- gas = hylo alg lag where
--   lag s = case project s of
--     Nil        -> EmpF
--     Cons h []  -> SinF h
--     Cons {}    ->
--       let (l, r) = splitAt (length s `div` 2) s
--       in  BinF l r
--
--   alg = \case
--     EmpF     -> lef
--     SinF a   -> one a
--     BinF l r -> mer l r
--
-- wyt :: Heap a -> Int
-- wyt = getSum . cata alg where
--   alg = \case
--     LeafF -> mempty
--     NodeF _ _ l r -> 1 <> l <> r
--
-- -- reference
--
-- nodF :: a -> Heap a -> Heap a -> HeapF a (Heap a)
-- nodF x l r
--   | ran l >= ran r = NodeF (1 <> ran r) x l r
--   | otherwise      = NodeF (1 <> ran l) x r l
--
-- nod :: a -> Heap a -> Heap a -> Heap a
-- nod x l r = Fix (nodF x l r)
--
-- mux :: Ord a => Heap a -> Heap a -> Heap a
-- mux l r = case (project l, project r) of
--   (_, LeafF) -> l
--   (LeafF, _) -> r
--   (NodeF _ m a b, NodeF _ n c d)
--     | m <= n    -> nod m a (mux b r)
--     | otherwise -> nod n c (mux l d)
--
-- oil :: Ord a => [a] -> Heap a
-- oil = cata $ \case
--   Nil      -> lef
--   Cons h t -> put h t
--
-- -- test
--
-- -- (2) 1
-- --   |     \
-- -- (1) 2   (1) 3
-- --   |  \    |    \
-- --   L   L (1) 4   L
-- --           |    \
-- --         (1) 5   L
-- --           |    \
-- --           L     L
--
-- test0 :: Heap Int
-- test0 = gas [1..5]
--
-- -- (1) 1
-- --   |   \
-- -- (1) 2  L
-- --   |   \
-- -- (1) 3  L
-- --   |   \
-- -- (1) 4  L
-- --   |   \
-- -- (1) 5  L
-- --   |   \
-- --   L    L
--
-- test1 :: Heap Int
-- test1 = oil [1..5]
