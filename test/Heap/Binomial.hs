{-# OPTIONS_GHC -fno-warn-orphans #-}

module Heap.Binomial where

import qualified Okasaki.Heap.Binomial as B
import Test.QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

-- binomial trees, heaps both have invariants

-- tree

-- * tree of rank r should have 2 ^ r nodes
-- * tree of rank r isbinomial  a node with r children, with each child
--   t_i having rank r - i
-- * children are maintained in decreasing order of rank
-- * elements are stored in heap order

-- heap

-- * no two trees have same rank
-- * list is in increasing order of rank

