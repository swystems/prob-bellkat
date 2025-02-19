{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Utils.Convex
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
    reduceConvexHull :: Set a -> Set a
    reduceConvexHull = id

instance Ord a => Convex (D a) where
    combine = D.djoin
    reduceConvexHull = id

instance Convex (SD a) where
    combine = D.sdjoin . toSubdistribution

newtype C a = C { unC :: Set a }
    deriving newtype (Foldable, Monoid, Eq, Ord)

createC :: Convex a => Set a -> C a
createC = C . reduceConvexHull 

instance (Convex a, Ord a) => Semigroup (C a) where
    (C xs) <> (C ys) = createC $ xs <> ys

instance (Ord a, Show a) => Show (C a) where
    show (C xs) = "⦅" <> intercalate "," (show <$> toList xs) <> "⦆"

instance (Convex a, Ord a) => GHC.Exts.IsList (C a) where
    type Item (C a) = a
    toList = toList . unC
    fromList = createC . fromList

instance Constrained C where
    type Dom C a = (Ord a, Convex a)

instance CFunctor C where
    cmap f (C xs) = createC $ cmap f xs

instance CPointed C where
    cpure = C . cpure

-- | Essentially weighted Minkowski sum
instance (Ord a, Dom C a) => Convex (C a) where
    combine = fromList . map (combine . fromList)
        . foldl' (\acc (ca, p) -> (:) <$> fmap (,p) (toList ca) <*> acc) [[]]
        . toList

newtype CD a = CD { unCD :: C (D a) } deriving newtype (Semigroup, Monoid, Show, Eq)

instance Ord a => GHC.Exts.IsList (CD a) where
    type Item (CD a) = D a
    toList = toList . unC . unCD
    fromList = CD . createC . fromList

instance Ord a => Convex (CD a) where
    combine = CD . combine . fmap unCD

instance Constrained CD where
    type Dom CD a = Ord a

instance CFunctor CD where
    cmap f = CD . cmap (fmap f) . unCD

instance CPointed CD where
    cpure = CD . cpure . pure

instance Foldable CD where
    foldMap f = foldMap (foldMap f) . unCD

instance CBind CD where
    (CD xs) >>- f = CD $ createC $ unC xs >>- (unC . unCD . combine . fmap f)

newtype CSD a = CSD { unCSD :: C (SD a) } deriving newtype (Semigroup, Monoid, Show, Eq, Ord)

instance Ord a => Convex (CSD a) where
    combine = CSD . combine . fmap unCSD

instance Ord a => GHC.Exts.IsList (CSD a) where
    type Item (CSD a) = SD a
    toList = toList . unC . unCSD
    fromList = CSD . createC . fromList
