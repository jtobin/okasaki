{-# OPTIONS_GHC -Wall -fno-warn-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Okasaki.Map (
    Map(..)
  ) where

import qualified Okasaki.Tree as T

data Per a b = Per !a b

instance (Show a, Show b) => Show (Per a b) where
  show (Per a b) = show (a, b)

instance Eq a => Eq (Per a b) where
  Per a _ == Per c _ = a == c

instance Ord a => Ord (Per a b) where
  compare (Per a _) (Per b _) = compare a b

-- exercise 2.6 (use tree to implement a finite map)
newtype Map k a = Map (T.Tree (Per k a))
  deriving Show

non :: Map k a
non = Map T.lef

-- NB does not replace elements
put :: (Ord k, Eq a) => k -> a -> Map k a -> Map k a
put k a (Map m) = Map $
  let per = Per k a
  in  T.rub per m

get :: Ord k => k -> Map k a -> Maybe a
get k (Map m) = do
  las <- T.spy (Per k undefined) m -- NB we only care about keys
  case las of
    Per s v
      | s == k    -> pure v
      | otherwise -> Nothing

