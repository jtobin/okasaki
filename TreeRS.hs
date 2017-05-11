{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Functor.Foldable
import Text.Show.Deriving

data TreeF a r =
    LeafF
  | NodeF r a r
  deriving (Functor, Show)

$(deriveShow1 ''TreeF)

type Tree a = Fix (TreeF a)

leaf :: Tree a
leaf = Fix LeafF

node :: a -> Tree a -> Tree a -> Tree a
node x l r = Fix (NodeF l x r)

-- insert :: forall a. Ord a => a -> Tree a -> Tree a
-- insert x = apo coalg where
--   coalg :: Ord a => Tree a -> TreeF a (Either (Tree a) (Tree a))
--   coalg input = case unfix input of
--     LeafF       -> NodeF (Left leaf) x (Left leaf)
--     NodeF l e r -> case compare x e of
--       EQ -> NodeF (Left l) e (Left r)
--       LT -> NodeF (Right l) e (Left r)
--       GT -> NodeF (Left l) e (Right r)

insert :: forall a. Ord a => a -> Tree a -> Tree a
insert x = apo coalg where
  coalg :: (Ord b, b ~ a) => Tree b -> TreeF a (Either (Tree b) (Tree b))
  coalg input = case unfix input of
    LeafF       -> NodeF (Left leaf) x (Left leaf)
    NodeF l e r -> case compare x e of
      EQ -> NodeF (Left l) e (Left r)
      LT -> NodeF (Right l) e (Left r)
      GT -> NodeF (Left l) e (Right r)

member :: Ord a => a -> Tree a -> Bool
member x = cata $ \case
  LeafF       -> False
  NodeF l e r -> case compare x e of
    EQ -> True
    LT -> l
    GT -> r

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert leaf

test0 :: Num a => Tree a
test0 = node 1 leaf leaf

-- 1
-- | \
-- L  L

test1 :: Num a => Tree a
test1 = node 2 (node 1 leaf leaf) (node 3 leaf leaf)

-- 2
-- |   \
-- 1    3
-- | \  | \
-- L  L L  L

test2 :: (Ord a, Num a) => Tree a
test2 = insert 0 test1

-- 2
-- |   \
-- 1    3
-- | \  | \
-- 0  L L  L
-- | \
-- L  L

test3 :: (Ord a, Num a) => Tree a
test3 = insert 5 test1

-- 2
-- |   \
-- 1    3
-- | \  | \
-- L  L L  5
--         | \
--         L  L

test4 :: (Ord a, Num a) => Tree a
test4 = insert 4 test3

-- 2
-- |   \
-- 1    3
-- | \  | \
-- L  L L  5
--         | \
--         4  L
--         | \
--         L  L

