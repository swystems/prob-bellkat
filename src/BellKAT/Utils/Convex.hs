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
    , CD'
    , CSD
    , CSD'
    , getGenerators
    , computeEventProbabilityRange
    ) where

import           Data.List
import qualified GHC.Exts (IsList)
import           GHC.Exts (fromList, toList, Item)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Subcategory.Functor
import           Control.Subcategory.Bind
import           Control.Subcategory.Pointed
import           Data.Kind
import           Data.Typeable
import qualified Data.Aeson as A

import           BellKAT.Utils.Distribution as D
import           BellKAT.Utils.Convex.DConvexHull

class Convex a where
    type ConvexP a :: Type 
    combine :: D (ConvexP a) a -> a
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

instance (RationalOrDouble p, Show p, Typeable a, Show a, Ord a) => Convex (D p a) where
    type ConvexP (D p a) = p
    combine = D.djoin
    reduceConvexHull = reduceConvexHullD

instance (Typeable a, RationalOrDouble p, Show p, Show a, Ord a) => ComputableConvex (D p a) where
    isInConvexHullOf = isInConvexHullOfD

instance (Fractional p, Ord p, Ord a) => Convex (SD p a) where
    type ConvexP (SD p a) = p
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
instance (Fractional (ConvexP a), Ord (ConvexP a), Ord a, Dom C a) => Convex (C a) where
    type ConvexP (C a) = ConvexP a
    combine = fromList . combineConvexSets . toList
      where
        combineConvexSets :: [(C a, ConvexP a)] -> [a]
        combineConvexSets = map (combine . fromList) . mapM (\(ca, p) -> (,p) <$> toList ca)

newtype CD p a = CD { unCD :: C (D p a) } deriving newtype (Semigroup, Monoid, Show, Eq, Ord, HasMemberC)

type CD' = CD Probability

getGenerators :: CD p a -> [D p a]
getGenerators = Set.toList . unC . unCD 

computeEventProbabilityRange :: (Num p, Ord p) => (a -> Bool) -> CD p a -> (p, p)
computeEventProbabilityRange ev cdp = 
    let minProb = minimum . map (computeEventProbability ev) . getGenerators $ cdp
        maxProb = maximum . map (computeEventProbability ev) . getGenerators $ cdp
     in (minProb, maxProb)

instance (Show a, Typeable a, Ord p, RationalOrDouble p, Ord a) => GHC.Exts.IsList (CD p a) where
    type Item (CD p a) = D p a
    toList = toList . unC . unCD
    fromList = CD . createC . fromList

instance (RationalOrDouble p, Typeable a, Show a, Ord a) => Convex (CD p a) where
    type ConvexP (CD p a) = p
    combine = CD . combine . cmap unCD

instance Constrained (CD p) where
    type Dom (CD p) a = (Show a, Ord a, Typeable a)

instance RationalOrDouble p => CFunctor (CD p) where
    cmap f = CD . cmap (cmap f) . unCD

instance RationalOrDouble p => CPointed (CD p) where
    cpure = CD . cpure . cpure

instance Foldable (CD p) where
    foldMap f = foldMap (foldMap f) . unCD

instance (RationalOrDouble p, Show p) => CBind (CD p) where
    cjoin = mconcat . map combine . toList

instance HasMapProbability CD where
    mapProbability f = CD . cmap (mapProbability f) . unCD 

newtype CSD p a = CSD { unCSD :: C (SD p a) } deriving newtype (Semigroup, Monoid, Show, Eq, Ord)

type CSD' = CSD Probability

instance (Fractional p, Ord p, Ord a) => Convex (CSD p a) where
    type ConvexP (CSD p a) = p
    combine = CSD . combine . cmap unCSD

instance (Fractional p, Ord p, Ord a) => GHC.Exts.IsList (CSD p a) where
    type Item (CSD p a) = SD p a
    toList = toList . unC . unCSD
    fromList = CSD . createC . fromList

instance (Show a, Ord a, Typeable a, A.ToJSON a, A.ToJSON p, RationalOrDouble p) 
        => A.ToJSON (CD p a) where
    toJSON = A.toJSON . toList

instance (Show a, Ord a, Typeable a, A.FromJSON a, A.FromJSON p, RationalOrDouble p) 
        => A.FromJSON (CD p a) where
    parseJSON = fmap fromList . A.parseJSON
