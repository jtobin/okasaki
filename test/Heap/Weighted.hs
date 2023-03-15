{-# OPTIONS_GHC -fno-warn-orphans #-}

module Heap.Weighted (
    bleftist
  , decorated
  , heap

  , tests
  ) where

import Control.Monad (foldM, replicateM)
import Data.Fix (Fix(..))
import Data.Functor.Foldable (project, para)
import qualified Okasaki.Heap.Leftist.Weighted as H
import Test.QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

tests :: [TestTree]
tests = [
    testProperty "decorations accurate" decorated
  , testProperty "weight-biased leftist property invariant" bleftist
  , testProperty "heap order invariant" heap
  ]


dec :: H.Heap a -> Bool
dec h = H.siz h == H.wyt h

wef :: H.Heap a -> Bool
wef = para $ \case
  H.LeafF -> True
  H.NodeF _ _ (l, c) (r, d) -> H.siz l >= H.siz r && c && d

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
  Put k -> pure (H.put k h)
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
    , [ Fix (H.NodeF (1 <> H.siz k <> H.siz s) b k s) |
        (b, k, s) <- (,,) <$> shrink a <*> lil l <*> lil r]
    ]

instance (Ord k, Arbitrary k) => Arbitrary (H.Heap k) where
  arbitrary = hep
  shrink = lil

decorated :: Property
decorated = forAllShrink (hep :: Gen (H.Heap Int)) lil dec

bleftist :: Property
bleftist = forAllShrink (hep :: Gen (H.Heap Int)) lil wef

heap :: Property
heap = forAllShrink (hep :: Gen (H.Heap Int)) lil hor
