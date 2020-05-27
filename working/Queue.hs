{-# OPTIONS_GHC -Wall #-}

module Queue where

import Prelude hiding (head, tail, reverse)
import qualified Prelude as Prelude (reverse)

data Queue a = Queue {
    forward :: [a]
  , reverse :: [a]
  } deriving Show

head :: Queue a -> Maybe a
head (Queue [] _)    = Nothing
head (Queue (h:_) _) = Just h

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

checkf :: Queue a -> Queue a
checkf (Queue [] r) = Queue (Prelude.reverse r) []
checkf q = q

snoc :: Queue a -> a -> Queue a
snoc (Queue f r) e = checkf (Queue f (e : r))

tail :: Queue a -> Queue a
tail (Queue (_:t) r) = checkf (Queue t r)
tail q = q

test :: Queue Int
test = Queue [1,2,3] [6,5,4]

