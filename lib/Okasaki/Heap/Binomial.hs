{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Okasaki.Heap.Binomial where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix hiding (cata, ana, hylo)
import Data.Foldable (toList)
import Data.Functor.Foldable
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Text.Show.Deriving

data TreeF a r = NodeF a [r]
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Tree a = Fix (TreeF a)

$(deriveShow1 ''TreeF)
$(deriveEq1 ''TreeF)

-- exercise 3.6: remove redundant rank info
data Bush a = Bush {-# UNPACK #-} !Int !(Tree a)
  deriving Show

newtype Heap a = Heap [Bush a]
  deriving Show

lef :: Heap a
lef = Heap []

nod :: a -> [Tree a] -> Tree a
nod a t = Fix (NodeF a t)

lin :: Ord a => Bush a -> Bush a -> Bush a
lin (Bush i s@(project -> NodeF a c)) (Bush j t@(project -> NodeF b d))
  | i /= j    = error "(okasaki, binomial): internal error"
  | a <= b    = Bush (succ i) (nod a (t : c))
  | otherwise = Bush (succ i) (nod b (s : d))

ran :: Bush a -> Int
ran (Bush i _) = i

roo :: Bush a -> a
roo (Bush _ (project -> NodeF a _)) = a

pet :: Ord a => Bush a -> [Bush a] -> [Bush a]
pet a as = toList $ cata alg as (a :| as)
  where
    alg l n@(t :| ts) = case l of
      Nil -> n
      Cons h f
        | ran t < ran h -> n
        | otherwise     -> f $! lin t h :| drop 1 ts

put :: Ord a => a -> Heap a -> Heap a
put a (Heap as) = Heap $
  pet (Bush 0 (nod a [])) as

mer :: Ord a => Heap a -> Heap a -> Heap a
mer (Heap a) (Heap b) = Heap $ mor a b where
  mor l r = case (l, r) of
    (p, []) -> p
    ([], q) -> q
    (h : t, i : s)
      | ran h < ran i -> h : mor t r
      | ran i < ran h -> i : mor l s
      | otherwise     -> pet (lin h i) (mor t s)

out :: Ord a => Heap a -> Maybe (Bush a, Heap a)
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
bed :: Ord a => Heap a -> Maybe a
bed (Heap a) = case sortOn roo a of
  []      -> Nothing
  (h : _) -> Just (roo h)

cut :: Ord a => Heap a -> Heap a
cut h = fromMaybe lef $ do
  (Bush i (project -> NodeF _ s), t) <- out h
  let p = fmap (Bush (pred i)) s
  pure $ mer (Heap (reverse p)) t

-- reference

-- arbitrary recursion
pyt :: Ord a => Bush a -> Heap a -> Heap a
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
