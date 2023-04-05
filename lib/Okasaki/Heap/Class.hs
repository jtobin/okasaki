
module Okasaki.Heap.Class where

import qualified Okasaki.Heap.Binomial as B
import qualified Okasaki.Heap.Leftist as L
import qualified Okasaki.Heap.Leftist.Weighted as W

class Heap (h :: * -> *) where
  bot :: Ord a => h a -> Maybe a
  put :: Ord a => a -> h a -> h a
  mer :: Ord a => h a -> h a -> h a
  cut :: Ord a => h a -> h a

instance Heap B.Heap where
  bot = B.bot
  put = B.put
  mer = B.mer
  cut = B.cut
