module BellKAT.Utils.Multiset 
    ( Multiset
    , fromList
    , isSubsetOf
    , toSet
    , toCountMap
    , singleton
    , map
    , filter
    , difference
    , count
    , min
    ) where

import           Prelude                    hiding (map, filter, min)
import           Data.List                  (intercalate)
import           Data.Foldable              (toList)
import           Data.Set                   (Set)
import           Data.Map.Strict            (Map)
import qualified Data.Multiset              as MsetOrig
import qualified Data.Aeson as A
import qualified GHC.Exts (IsList, Item, fromList, toList)

newtype Multiset a = MS { unMS :: MsetOrig.Multiset a } 
    deriving newtype (Eq, Ord, Foldable, Semigroup, Monoid)

instance Ord a => GHC.Exts.IsList (Multiset a) where
    type Item (Multiset a) = a
    fromList = MS . GHC.Exts.fromList 
    toList (MS xs) = GHC.Exts.toList xs

instance Show a => Show (Multiset a) where
    show (MS a) = "⦃" <> intercalate "," (show <$> toList a) <> "⦄"

toSet :: Multiset a -> Set a
toSet = MsetOrig.toSet . unMS

toCountMap :: Multiset a -> Map a Int
toCountMap = MsetOrig.toCountMap . unMS

isSubsetOf :: Ord a => Multiset a -> Multiset a -> Bool
isSubsetOf (MS x) (MS y) = x `MsetOrig.isSubsetOf` y

fromList :: Ord a => [a] -> Multiset a
fromList = MS . MsetOrig.fromList

singleton :: a -> Multiset a
singleton = MS . MsetOrig.singleton

map :: (Ord a, Ord b) => (a -> b) -> Multiset a -> Multiset b
map f = MS . MsetOrig.map f . unMS

filter :: Ord a => (a -> Bool) -> Multiset a -> Multiset a
filter f = MS . MsetOrig.filter f . unMS

difference :: Ord a => Multiset a -> Multiset a -> Multiset a
difference (MS x) (MS y) = MS $ MsetOrig.difference x y

count :: Ord a => a -> Multiset a -> Int
count x = MsetOrig.count x . unMS

min :: Ord a => Multiset a -> Multiset a -> Multiset a
min (MS x) (MS y) = MS $ MsetOrig.min x y

instance A.ToJSON a => A.ToJSON (Multiset a) where
    toJSON = A.toJSON . toList

instance (Ord a, A.FromJSON a) => A.FromJSON (Multiset a) where
    parseJSON = fmap GHC.Exts.fromList . A.parseJSON
