{-# OPTIONS_GHC -Wall #-}

module Okasaki.Heap.Class where

import Data.Kind (Type)
import Data.Fix (Fix(..))
import qualified Okasaki.Heap.Binomial as B
import qualified Okasaki.Heap.Leftist as L
import qualified Okasaki.Heap.Leftist.Weighted as W

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
    deriving Show

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

instance Heap L.Heap where
  data Pile L.Heap a =
      Nil
    | Lib a (L.Heap a)
    deriving Show

  via = \case
    L.Heap (Fix L.LeafF) -> Nil
    h -> let b = L.bot h
         in  case b of
               Nothing -> Nil
               Just a  -> Lib a h

  bot = \case
    Nil     -> Nothing
    Lib a _ -> Just a

  put a = \case
    Nil -> Lib a (L.put a L.lef)
    Lib m h
      | a < m     -> Lib a (L.put a h)
      | otherwise -> Lib m (L.put a h)

  cut h = case h of
    Nil -> Nil
    Lib _ t ->
      let c = L.cut t
      in  case L.bot c of
            Nothing -> Nil
            Just a  -> Lib a c

  mer h l = case (h, l) of
    (Nil, _) -> l
    (_, Nil) -> h
    (Lib a s, Lib b t) -> Lib (min a b) (L.mer s t)

  out = \case
    Nil -> L.lef
    Lib _ h -> h

instance Heap W.Heap where
  data Pile W.Heap a =
      Net
    | Win a (W.Heap a)
    deriving Show

  via = \case
    W.Heap (Fix W.LeafF) -> Net
    h -> let b = W.bot h
         in  case b of
               Nothing -> Net
               Just a  -> Win a h

  bot = \case
    Net     -> Nothing
    Win a _ -> Just a

  put a = \case
    Net -> Win a (W.put a W.lef)
    Win m h
      | a < m     -> Win a (W.put a h)
      | otherwise -> Win m (W.put a h)

  cut h = case h of
    Net -> Net
    Win _ t ->
      let c = W.cut t
      in  case W.bot c of
            Nothing -> Net
            Just a  -> Win a c

  mer h l = case (h, l) of
    (Net, _) -> l
    (_, Net) -> h
    (Win a s, Win b t) -> Win (min a b) (W.mer s t)

  out = \case
    Net -> W.lef
    Win _ h -> h

