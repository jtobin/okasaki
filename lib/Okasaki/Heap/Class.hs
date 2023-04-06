{-# OPTIONS_GHC -Wall #-}

module Okasaki.Heap.Class where

import Data.Kind (Type)
import qualified Okasaki.Heap.Binomial as B

-- NB these need to be newtyped to be made proper instances
-- import qualified Okasaki.Heap.Leftist as L
-- import qualified Okasaki.Heap.Leftist.Weighted as W

-- exercise 3.7: generic explicit-min heap
class Heap (h :: Type -> Type) where
  data Pile h :: Type -> Type

  via :: Ord a => h a -> Pile h a
  bot :: Ord a => Pile h a -> Maybe a
  put :: Ord a => a -> Pile h a -> Pile h a
  cut :: Ord a => Pile h a -> Pile h a
  mer :: Ord a => Pile h a -> Pile h a -> Pile h a
  out :: Pile h a -> h a

instance Heap B.Heap where
  data Pile B.Heap a =
      Nib
    | Bin a (B.Heap a)

  via = \case
    B.Heap [] -> Nib
    h -> let b = B.bot h
         in  case b of
               Nothing -> Nib
               Just a  -> Bin a h

  bot = \case
    Nib     -> Nothing
    Bin a _ -> Just a

  put a = \case
    Nib -> Bin a (B.put a B.lef)
    Bin m h
      | a < m     -> Bin a (B.put a h)
      | otherwise -> Bin m (B.put a h)

  cut h = case h of
    Nib -> Nib
    Bin _ t ->
      let c = B.cut t
      in  case B.bot c of
            Nothing -> Nib
            Just a  -> Bin a c

  mer h l = case (h, l) of
    (Nib, _) -> l
    (_, Nib) -> h
    (Bin a s, Bin b t) -> Bin (min a b) (B.mer s t)

  out = \case
    Nib -> B.lef
    Bin _ h -> h

