{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Okasaki.Stack (
    StackF(..)
  , Stack
  , empty
  , push
  , pop

  , fromList
  , toList
  , isEmpty
  , cat
  , update
  , suffixes
  ) where

import Prelude hiding (head, tail)
import Data.Functor.Foldable as RS
import Text.Show.Deriving

data StackF a r =
    NilF
  | ConsF !a r
  deriving (Eq, Functor, Foldable, Traversable, Show)

$(deriveShow1 ''StackF)

type Stack a = Fix (StackF a)

empty :: Stack a
empty = Fix NilF

push :: a -> Stack a -> Stack a
push h t = Fix (ConsF h t)

pop :: Stack a -> Maybe (a, Stack a)
pop s = case project s of
  NilF      -> Nothing
  ConsF h t -> Just (h, t)

fromList :: [a] -> Stack a
fromList = ana coalg where
  coalg = \case
    []      -> NilF
    (h : t) -> ConsF h t

toList :: Stack a -> [a]
toList = ana coalg where
  coalg s = case project s of
    NilF      -> Nil
    ConsF h t -> Cons h t

isEmpty :: Stack a -> Bool
isEmpty s = case project s of
  NilF -> True
  _    -> False

cat :: Stack a -> Stack a -> Stack a
cat l r = apo coalg (project l) where
  coalg = \case
    ConsF h t -> case project t of
      NilF -> ConsF h (Left r)
      rest -> ConsF h (Right rest)

    NilF -> fmap Left (project r)

update :: Int -> a -> Stack a -> Stack a
update idx x s = apo coalg (idx, s) where
  coalg (j, stack) = case project stack of
    NilF      -> NilF
    ConsF h t ->
      if   j <= 0
      then ConsF x (Left t)
      else ConsF h (Right (pred j, t))

-- exercise 2.1
suffixes :: Stack a -> Stack (Stack a)
suffixes = ana coalg where
  coalg stack = case project stack of
    NilF      -> NilF
    ConsF _ t -> ConsF t t

-- test

test0 :: Stack Int
test0 = fromList [1..3]

test1 :: Stack Int
test1 = fromList [4..7]

test2 :: Stack Int
test2 = update 3 100 (cat test0 test1)
