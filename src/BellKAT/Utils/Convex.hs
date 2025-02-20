{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module BellKAT.Utils.Convex
    ( C
    , memberC
    , notMemberC
    , isValidC
    , Convex (..)
    , CD
    , CSD
    ) where

import           Data.List
import qualified GHC.Exts (IsList)
import           GHC.Exts (fromList, toList, Item)
import           Data.Set (Set)
import           Control.Subcategory.Functor
import           Control.Subcategory.Bind
import           Control.Subcategory.Pointed

import           BellKAT.Utils.Distribution as D
import           BellKAT.Utils.Convex.DConvexHull

class Convex a where
    combine :: D a -> a
    reduceConvexHull :: Set a -> Set a
    reduceConvexHull = id

class Convex a => ComputableConvex a where
    isInConvexHullOf :: a -> Set a -> Bool

class GHC.Exts.IsList a => HasMemberC a where
    memberC :: Item a -> a -> Bool

notMemberC :: HasMemberC a => Item a -> a -> Bool
x `notMemberC` xs = not $ x `memberC` xs

isValidC :: HasMemberC a => a -> Bool
isValidC xs = helper xs [] (toList xs)
  where
    helper _ _ [] = True
    helper (p :: a) acc (x:xs') = (x `notMemberC` fromList @a xs')  && helper p (x:acc) xs'

instance (Ord a) => Convex (D a) where
    combine = D.djoin
    reduceConvexHull = reduceConvexHullD

instance (Ord a) => ComputableConvex (D a) where
    isInConvexHullOf = isInConvexHullOfD

instance Ord a => Convex (SD a) where
    combine = D.sdjoin . toSubdistribution

newtype C a = C { unC :: Set a }
    deriving newtype (Foldable, Eq, Ord)

createC :: Convex a => Set a -> C a
createC = C . reduceConvexHull

instance (Ord a, ComputableConvex a) => HasMemberC (C a) where
    x `memberC` (C xs) = x `isInConvexHullOf` xs

instance (Convex a, Ord a) => Semigroup (C a) where
    (C xs) <> (C ys) = createC $ xs <> ys

instance (Convex a, Ord a) => Monoid (C a) where
    mempty = C mempty

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
    cpure = createC . cpure

-- | Essentially weighted Minkowski sum
instance (Ord a, Dom C a) => Convex (C a) where
    combine = fromList . combineConvexSets . toList
      where
        combineConvexSets :: [(C a, Probability)] -> [a]
        combineConvexSets = map (combine . fromList) . mapM (\(ca, p) -> (,p) <$> toList ca)

newtype CD a = CD { unCD :: C (D a) } deriving newtype (Semigroup, Monoid, Show, Eq, Ord, HasMemberC)

instance (Show a, Ord a) => GHC.Exts.IsList (CD a) where
    type Item (CD a) = D a
    toList = toList . unC . unCD
    fromList = CD . createC . fromList

instance Ord a => Convex (CD a) where
    combine = CD . combine . cmap unCD

instance Constrained CD where
    type Dom CD a = (Show a, Ord a)

instance CFunctor CD where
    cmap f = CD . cmap (cmap f) . unCD

instance CPointed CD where
    cpure = CD . cpure . cpure

instance Foldable CD where
    foldMap f = foldMap (foldMap f) . unCD

instance CBind CD where
    cjoin = mconcat . map combine . toList

newtype CSD a = CSD { unCSD :: C (SD a) } deriving newtype (Semigroup, Monoid, Show, Eq, Ord)

instance Ord a => Convex (CSD a) where
    combine = CSD . combine . cmap unCSD

instance Ord a => GHC.Exts.IsList (CSD a) where
    type Item (CSD a) = SD a
    toList = toList . unC . unCSD
    fromList = CSD . createC . fromList
