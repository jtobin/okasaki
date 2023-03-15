{-# OPTIONS_GHC -fno-warn-orphans #-}

module Heap.Leftist (
    decorated
  , leftist
  , heap
  , size

  , tests
  ) where

import Control.Monad (foldM, replicateM)
import Data.Functor.Foldable (project, cata, para)
import Data.Monoid
import qualified Okasaki.Heap.Leftist as H
import Test.QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

tests :: [TestTree]
tests = [
    testProperty "decorations accurate" decorated
  , testProperty "leftist property invariant" leftist
  , testProperty "heap order invariant" heap
  , testProperty "correct right-spine length" size
  ]

spi :: H.Heap a -> Int
spi = cata $ \case
  H.LeafF         -> 0
  H.NodeF _ _ _ r -> succ r

mos :: H.Heap a -> Bool
mos h =
  let n = H.wyt h
  in  spi h <= floor (logBase 2 ((fromIntegral (succ n)) :: Double))

dec :: H.Heap a -> Bool
dec h = getSum (H.ran h) == spi h

wef :: H.Heap a -> Bool
wef = para $ \case
  H.LeafF -> True
  H.NodeF _ _ (l, c) (r, d) -> H.ran l >= H.ran r && c && d

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
  num  <- choose (0, 1000)
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

decorated :: Property
decorated = forAllShrink (hep :: Gen (H.Heap Int)) lil dec

leftist :: Property
leftist = forAllShrink (hep :: Gen (H.Heap Int)) lil wef

heap :: Property
heap = forAllShrink (hep :: Gen (H.Heap Int)) lil hor

size :: Property
size = forAllShrink (hep :: Gen (H.Heap Int)) lil mos
