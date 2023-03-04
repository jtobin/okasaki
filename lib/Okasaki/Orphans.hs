{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Okasaki.Orphans where

import Data.Functor.Const
import Data.Functor.Foldable
import qualified Data.Monoid as M

type instance Base (M.Last a) = Const (M.Last a)

instance Corecursive (M.Last a) where
  embed (Const s) = s

instance Recursive (M.Last a) where
  project = Const

