{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Utils.ConvexSetOfDistributions
    ( C
    , Convex (..)
    , CD
    , CSD
    ) where

import           Data.List
import qualified GHC.Exts (IsList, Item)
import           GHC.Exts (fromList, toList)
import           Data.Set (Set)
import           Control.Subcategory.Functor
import           Control.Subcategory.Bind
import           Control.Subcategory.Pointed

import           BellKAT.Utils.Distribution as D hiding (norm)

class Convex a where
    combine :: D a -> a

instance Convex (D a) where
    combine = D.djoin

instance Convex (SD a) where
    combine = D.sdjoin . toSubdistribution

newtype C a = C { unC :: Set a }
    deriving newtype (Foldable, Monoid)

instance (Show a, Ord a) => Eq (C a) where
    (C xs) == (C ys) = xs == ys

instance Ord a => Semigroup (C a) where
    (C xs) <> (C ys) = C $ xs <> ys

instance (Ord a, Show a) => Show (C a) where
    show (C xs) = "⦅" <> intercalate "," (show <$> toList xs) <> "⦆"

instance Ord a => GHC.Exts.IsList (C a) where
    type Item (C a) = a
    toList = toList . unC
    fromList = C . fromList

instance Constrained C where
    type Dom C a = Ord a

instance CFunctor C where
    cmap f (C xs) = C $ cmap f xs

instance CPointed C where
    cpure = C . cpure

-- | Essentially weighted Minkowski sum
instance (Dom C a, Convex a) => Convex (C a) where
    combine = fromList . map (combine . fromList)
        . foldl' (\acc (ca, p) -> (:) <$> fmap (,p) (toList ca) <*> acc) [[]]
        . toList

newtype CD a = CD { unCD :: C (D a) } deriving newtype (Convex, Semigroup, Monoid, Show, Eq)

instance Ord a => GHC.Exts.IsList (CD a) where
    type Item (CD a) = D a
    toList = toList . unC . unCD
    fromList = CD . C . fromList

instance Constrained CD where
    type Dom CD a = Ord a

instance CFunctor CD where
    cmap f = CD . cmap (fmap f) . unCD

instance CPointed CD where
    cpure = CD . cpure . pure

instance Foldable CD where
    foldMap f = foldMap (foldMap (f . fst) . toList) . unCD

instance CBind CD where
    (CD xs) >>- f = CD $ C $ unC xs >>- (unC . unCD . combine . fmap f)

newtype CSD a = CSD { unCSD :: C (SD a) } deriving newtype (Convex, Semigroup, Monoid, Show, Eq)

instance Ord a => GHC.Exts.IsList (CSD a) where
    type Item (CSD a) = SD a
    toList = toList . unC . unCSD
    fromList = CSD . C . fromList
