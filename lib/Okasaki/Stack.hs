{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}

module Okasaki.Stack (
    StackF(..)
  , Stack
  , nil
  , put
  , pop

  , gas
  , tap
  , non
  , cat
  , jab
  , suf
  ) where

import Prelude hiding (map)
import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix(..))
import Data.Functor.Foldable as RS
import Text.Show.Deriving

-- NB should arguably be made strict

data StackF a r =
    NilF
  | ConsF !a r
  deriving (Eq, Functor, Foldable, Traversable, Show)

$(deriveShow1 ''StackF)
$(deriveEq1 ''StackF)

type Stack a = Fix (StackF a)

-- | O(1)
nil :: Stack a
nil = Fix NilF

-- | O(1)
put :: a -> Stack a -> Stack a
put h t = Fix (ConsF h t)

-- | O(1)
pop :: Stack a -> Maybe (a, Stack a)
pop s = case project s of
  NilF      -> Nothing
  ConsF h t -> Just (h, t)

-- | O(n) for n = length input
gas :: [a] -> Stack a
gas = ana $ \case
  []      -> NilF
  (h : t) -> ConsF h t

-- | O(n) for n = length input
map :: (a -> b) -> Stack a -> Stack b
map f = ana lag where
  lag s = case project s of
    NilF      -> NilF
    ConsF h t -> ConsF (f h) t

-- | O(n) for n = length input
run :: (a -> b -> b) -> b -> Stack a -> b
run f o = cata $ \case
  NilF      -> o
  ConsF h t -> f h t

-- | O(n) for n = length input
tap :: Stack a -> [a]
tap = ana lag where
  lag s = case project s of
    NilF      -> Nil
    ConsF h t -> Cons h t

-- | O(1)
non :: Stack a -> Bool
non s = case project s of
  NilF -> True
  _    -> False

-- | O(n) for n = length l
cat :: Stack a -> Stack a -> Stack a
cat l r = apo lag (project l) where
  lag = \case
    NilF -> fmap Left (project r)
    ConsF h t -> case project t of
      NilF -> ConsF h (Left r)
      rest -> ConsF h (Right rest)

-- | O(n)
jab :: Int -> a -> Stack a -> Stack a
jab n x s = apo lag (n, s) where
  lag (j, tac) = case project tac of
    NilF -> NilF
    ConsF h t
      | j <= 0    -> ConsF x (Left t)
      | otherwise -> ConsF h (Right (pred j, t))

-- exercise 2.1

-- | O(n) for n = length input
suf :: Stack a -> Stack (Stack a)
suf = ana lag where
  lag tac = case project tac of
    NilF      -> NilF
    ConsF _ t -> ConsF t t

-- test

test0 :: Stack Int
test0 = gas [1..3]

test1 :: Stack Int
test1 = gas [4..7]

test2 :: Stack Int
test2 = jab 3 100 (cat test0 test1)
