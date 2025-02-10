{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}
module BellKAT.Utils.UnorderedTree where

import           Data.Foldable   (toList)
import           Data.Tree       (Forest, Tree)
import qualified Data.Tree       as OrdTree
import           Test.QuickCheck

import BellKAT.Utils.Multiset (Multiset)
import qualified BellKAT.Utils.Multiset as Mset

data UTree a = Node { rootLabel :: a, subForest :: Multiset (UTree a) }
    deriving stock (Eq, Ord)

instance Show a => Show (UTree a) where
    show = show . toTree

type UForest a = Multiset (UTree a)

fromTree :: (Ord a) => Tree a -> UTree a
fromTree (OrdTree.Node x xs) = Node x $ Mset.fromList (map fromTree xs)

toTree :: UTree a -> Tree a
toTree (Node x xs) = OrdTree.Node x (map toTree $ toList xs)

toForest :: UForest a -> Forest a
toForest = map toTree . toList

hasRoot :: (Eq a) => a -> UTree a -> Bool
hasRoot p = (== p) . rootLabel

instance (Arbitrary a, Ord a) => Arbitrary (Multiset a) where
    arbitrary = Mset.fromList <$> arbitrary
    shrink = fmap Mset.fromList . shrink . toList

instance (Arbitrary a, Ord a) => Arbitrary (UTree a) where
    arbitrary = fromTree <$> arbitrary
    shrink (Node a as) = fmap (Node a) (shrink as) ++ concatMap shrink as
