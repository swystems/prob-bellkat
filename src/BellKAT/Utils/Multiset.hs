{-# LANGUAGE OverloadedStrings #-}
module BellKAT.Utils.Multiset 
    ( Multiset
    , LabelledMultiset(..)
    , fromList
    , fromList'
    , isSubsetOf
    , isSubsetOf'
    , toSet
    , toSet'
    , toCountMap
    , toCountMap'
    , singleton
    , singleton'
    , singletonT
    , map
    , map'
    , filter
    , difference
    , count
    , count'
    , min
    , member
    , remove
    , (@)
    , bellPairs
    , labelledBellPairs
    , labelledMempty
    ) where

import           Prelude                    hiding (map, filter, min)
import           Data.List                  (intercalate)
import           Data.Foldable              (toList)
import           Data.Set                   (Set)
import           Data.Map.Strict            (Map)
import qualified Data.Multiset              as MsetOrig
import qualified Data.Aeson as A
import           Data.Aeson ((.=), (.:))
import qualified GHC.Exts (IsList, Item, fromList, toList)

newtype Multiset a = MS { unMS :: MsetOrig.Multiset a } 
    deriving newtype (Eq, Ord, Foldable, Semigroup, Monoid)

instance Ord a => GHC.Exts.IsList (Multiset a) where
    type Item (Multiset a) = a
    fromList = MS . GHC.Exts.fromList 
    toList (MS xs) = GHC.Exts.toList xs

instance Show a => Show (Multiset a) where
    show (MS a) = "⦃" <> intercalate "," (show <$> toList a) <> "⦄"

newtype LabelledMultiset t a = LMS { unLMS :: (Multiset a, t) }
    deriving newtype (Eq, Ord, Semigroup, Monoid)


instance Ord a => GHC.Exts.IsList (LabelledMultiset () a) where
    type Item (LabelledMultiset () a) = a
    fromList xs = fromList xs @ ()
    toList lms  = GHC.Exts.toList (bellPairs lms)

instance (Show t, Show a) => Show (LabelledMultiset t a) where
    show (LMS (ms, t)) = show ms ++ "@" ++ show t

-- | Attaches tag to a Multiset
(@) :: Multiset a -> t -> LabelledMultiset t a
(@) ms t = LMS (ms, t)

bellPairs        :: LabelledMultiset t a -> Multiset a
bellPairs        = fst . unLMS

labelledBellPairs :: LabelledMultiset t a -> t
labelledBellPairs = snd . unLMS

labelledMempty :: t -> LabelledMultiset t a
labelledMempty t = LMS (MS MsetOrig.empty, t)

toSet :: Multiset a -> Set a
toSet = MsetOrig.toSet . unMS

toSet' :: Ord a => LabelledMultiset t a -> Set a
toSet' = toSet . bellPairs

toCountMap :: Multiset a -> Map a Int
toCountMap = MsetOrig.toCountMap . unMS

toCountMap' :: Ord a => LabelledMultiset t a -> Map a Int
toCountMap' = toCountMap . bellPairs

isSubsetOf :: Ord a => Multiset a -> Multiset a -> Bool
isSubsetOf (MS x) (MS y) = x `MsetOrig.isSubsetOf` y

isSubsetOf' :: Ord a
            => LabelledMultiset l a
            -> LabelledMultiset l a
            -> Bool
isSubsetOf' a b = isSubsetOf (bellPairs a) (bellPairs b)

fromList :: Ord a => [a] -> Multiset a
fromList = MS . MsetOrig.fromList

fromList' :: Ord a => [a] -> LabelledMultiset () a
fromList' = LMS . (\x -> (fromList x, ()))

singleton :: a -> Multiset a
singleton = MS . MsetOrig.singleton

singleton' :: a -> LabelledMultiset () a
singleton' = LMS . (\x -> (singleton x, ()))

singletonT :: a -> t -> LabelledMultiset t a
singletonT x t = singleton x @ t

map :: (Ord a, Ord b) => (a -> b) -> Multiset a -> Multiset b
map f = MS . MsetOrig.map f . unMS

map' :: (Ord a, Ord b) => (a -> b) -> LabelledMultiset t a -> LabelledMultiset () b
map' f (LMS (ms, _)) = LMS (map f ms, ())

filter :: Ord a => (a -> Bool) -> Multiset a -> Multiset a
filter f = MS . MsetOrig.filter f . unMS

difference :: Ord a => Multiset a -> Multiset a -> Multiset a
difference (MS x) (MS y) = MS $ MsetOrig.difference x y

count :: Ord a => a -> Multiset a -> Int
count x = MsetOrig.count x . unMS

count' :: Ord a => a -> LabelledMultiset t a -> Int
count' x = count x . bellPairs

min :: Ord a => Multiset a -> Multiset a -> Multiset a
min (MS x) (MS y) = MS $ MsetOrig.min x y

member :: Ord a => a -> Multiset a -> Bool
member x = MsetOrig.member x . unMS

remove :: Ord a => a -> Multiset a -> Multiset a
remove x = MS . MsetOrig.remove x . unMS

instance A.ToJSON a => A.ToJSON (Multiset a) where
    toJSON = A.toJSON . toList

instance (Ord a, A.FromJSON a) => A.FromJSON (Multiset a) where
    parseJSON = fmap GHC.Exts.fromList . A.parseJSON

instance (A.ToJSON t, A.ToJSON a) => A.ToJSON (LabelledMultiset t a) where
    toJSON (LMS (ms, t)) = A.object ["bellPairs" .= toList ms, "label" .= t]

instance (Ord a, A.FromJSON t, A.FromJSON a) => A.FromJSON (LabelledMultiset t a) where
    parseJSON = A.withObject "LabelledMultiset" $ \o -> do
        bps <- o .: "bellPairs"
        lbl <- o .: "label"
        pure (LMS (GHC.Exts.fromList bps, lbl))
