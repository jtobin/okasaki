{-# OPTIONS_GHC -Wall #-}

module Dequeue where

import Queue (Queue(..))
import qualified Queue
import Prelude hiding (head, tail, reverse, last, init)
import qualified Prelude (reverse)

-- exercise 5.1a (double-ended queue)
newtype Dequeue a = Dequeue { dequeue :: Queue a } deriving Show

head :: Dequeue a -> Maybe a
head (Dequeue q) = Queue.head q

empty :: Dequeue a
empty = Dequeue Queue.empty

isEmpty :: Dequeue a -> Bool
isEmpty (Dequeue q) = Queue.isEmpty q

checkSym :: Dequeue a -> Dequeue a
checkSym (Dequeue q) = Dequeue (checkSymQ q)

checkSymQ :: Queue a -> Queue a
checkSymQ (Queue [] [r]) = Queue [r] []
checkSymQ (Queue []  r) = let (a, b) = splitter r in Queue (Prelude.reverse a) b
checkSymQ (Queue f  []) = let (a, b) = splitter f in Queue b (Prelude.reverse a)
checkSymQ e = e

splitter :: [a] -> ([a], [a])
splitter r =
  let m = length r
      n = let l = m `quot` 2 in if even m then l else succ l
  in  (drop n r, take n r)

snoc :: Dequeue a -> a -> Dequeue a
snoc (Dequeue (Queue f r)) e = Dequeue $ checkSymQ (Queue f (e : r))

cons :: Dequeue a -> a -> Dequeue a
cons (Dequeue (Queue f r)) e = Dequeue $ checkSymQ (Queue (e : f) r)

tail :: Dequeue a -> Dequeue a
tail (Dequeue (Queue (_:t) r)) = Dequeue $ checkSymQ (Queue t r)
tail deq = deq

last :: Dequeue a -> Maybe a
last (Dequeue (Queue _ (l:_)))  = Just l
last (Dequeue (Queue (l:_) [])) = Just l
last (Dequeue (Queue [] []))    = Nothing

init :: Dequeue a -> Dequeue a
init (Dequeue (Queue f (_:t))) = Dequeue $ checkSymQ (Queue f t)
init (Dequeue (Queue [_] []))  = empty
init deq = deq

test :: Dequeue Int
test = Dequeue Queue.test

