{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}

module Okasaki.Stack.CPS (
    StackF(..)
  , Stack
  , nil
  , put
  , pop
  , jab
  , non

  , map
  , run

  , gas
  , tap
  , cat
  , suf
  ) where

import Data.Fix hiding (cata, ana, hylo)
import Data.Functor.Foldable
import Prelude hiding (map)

newtype StackF a r = StackF (forall e. e -> (a -> r -> e) -> e)
  deriving Functor

type Stack a = Fix (StackF a)

nilF :: StackF a r
nilF = StackF const

putF :: a -> r -> StackF a r
putF h t = StackF (\_ c -> c h t)

-- | O(1)
nil :: Stack a
nil = Fix nilF

-- | O(1)
put :: a -> Stack a -> Stack a
put h t = Fix (putF h t)

-- | O(1)
pop :: Stack a -> Maybe (a, Stack a)
pop (project -> StackF c) = c Nothing b where
  b h t = Just (h, t)

-- | O(1)
non :: Stack a -> Bool
non (project -> StackF c) = c True (\_ _ -> False)

-- | O(n) for n = length input
gas :: [a] -> Stack a
gas = ana $ \case
  []      -> nilF
  (h : t) -> putF h t

-- | O(n) for n = length input
tap :: Stack a -> [a]
tap = ana lag where
  lag (project -> StackF c) = c Nil Cons

-- | O(n) for n = length input
map :: (a -> b) -> Stack a -> Stack b
map f = ana lag where
  lag (project -> StackF c) = c nilF b
  b h t = putF (f h) t

-- | O(n) for n = length input
run :: (a -> b -> b) -> b -> Stack a -> b
run f o = cata $ \case
  StackF c -> c o f

-- | O(n) for n = length l
cat :: Stack a -> Stack a -> Stack a
cat l r = apo lag (project l) where
  lag (StackF c) = c a b

  a = fmap Left (project r)

  b h (project -> StackF c) =
    let d     = putF h (Left r)
        e f g = putF h (Right (putF f g))
    in  c d e

-- | O(n)
jab :: Int -> a -> Stack a -> Stack a
jab n x s = apo lag (n, s) where
  lag (j, project -> StackF c) = c nilF (b j)

  b j h t
    | j <= 0    = putF x (Left t)
    | otherwise = putF h (Right (pred j, t))

-- exercise 2.1

-- | O(n) for n = length input
suf :: Stack a -> Stack (Stack a)
suf = ana lag where
  lag (project -> StackF c) = c nilF b
  b _ t = putF t t

-- test

test0 :: Stack Int
test0 = gas [1..3]

test1 :: Stack Int
test1 = gas [4..7]

test2 :: Stack Int
test2 = jab 3 100 (cat test0 test1)
