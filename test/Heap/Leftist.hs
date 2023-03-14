{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Heap.Leftist (
    leftist
  , heap
  ) where

import Control.Monad (foldM, replicateM)
import Data.Functor.Foldable (project, cata, para)
import Data.Monoid
import qualified Okasaki.Heap.Leftist as H
import Test.QuickCheck

lef :: H.Heap a -> Bool
lef h = getSum (H.ran h) == cata alg h where
  alg = \case
    H.LeafF         -> 0
    H.NodeF _ _ _ r -> succ r

hor :: Ord a => H.Heap a -> Bool
hor = para $ \case
  H.LeafF -> True
  H.NodeF _ a (l, c) (r, d) -> case (project l, project r) of
    (H.LeafF, H.LeafF)                 -> True && c && d
    (H.LeafF, H.NodeF _ v _ _)         -> a <= v && c && d
    (H.NodeF _ v _ _, H.LeafF)         -> a <= v && c && d
    (H.NodeF _ u _ _, H.NodeF _ v _ _) -> a <= u && a <= v && c && d

data Act k =
    Put k
  | Cut
  deriving (Eq, Show)

act :: Arbitrary k => Gen (Act k)
act = frequency [
    (10, Put <$> arbitrary)
  , (2, pure Cut)
  ]

use :: Ord k => Act k -> H.Heap k -> Gen (H.Heap k)
use a h = case a of
  Put k -> pure (H.pat k h)
  Cut   -> pure (H.cut h)

hep :: (Ord k, Arbitrary k) => Gen (H.Heap k)
hep = do
  num  <- choose (0, 10)
  acts <- replicateM num act
  foldM (flip use) H.lef acts

lil :: Arbitrary k => H.Heap k -> [H.Heap k]
lil h = case project h of
  H.LeafF -> mempty
  H.NodeF _ a l r -> mconcat [
      [H.lef]
    , [l, r]
    , [H.set b k s | (b, k, s) <- (,,) <$> shrink a <*> lil l <*> lil r]
    ]

instance (Ord k, Arbitrary k) => Arbitrary (H.Heap k) where
  arbitrary = hep
  shrink = lil

leftist :: Property
leftist = forAllShrink (hep :: Gen (H.Heap Int)) lil lef

heap :: Property
heap = forAllShrink (hep :: Gen (H.Heap Int)) lil hor
