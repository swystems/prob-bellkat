{-# LANGUAGE TupleSections #-}
module BellKAT.Utils.ConvexSetOfDistributions
    ( C
    , Convex (..)
    , CD
    , CSD
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.List
import qualified GHC.Exts (IsList, Item)
import           GHC.Exts (fromList, toList)
import           Data.Containers.ListUtils

import           BellKAT.Utils.Distribution as D

class Convex a where
    combine :: D a -> a

instance Convex (D a) where
    combine = D.djoin

instance Convex (SD a) where
    combine = D.sdjoin . toSubdistribution

newtype C a = C { unC :: [a] }
    deriving newtype (Functor, Applicative, Foldable, Semigroup, Monoid)

instance (Show a, Ord a) => Eq (C a) where
    (C xs) == (C ys) = sort (nubOrd xs) == sort (nubOrd ys)

instance (Ord a, Show a) => Show (C a) where
    show (C xs) = "⦅" <> intercalate "," (show <$> sort (nubOrd xs)) <> "⦆"

instance Ord a => GHC.Exts.IsList (C a) where
    type Item (C a) = a
    toList = unC
    fromList = C . nubOrd

-- | Essentially weighted Minkowski sum
instance Convex a => Convex (C a) where
    combine = fmap (combine . fromList)
        . foldl' (\acc (ca, p) -> (:) <$> fmap (,p) ca <*> acc) (pure [])
        . toList

newtype CD a = CD { unCD :: C (D a) } deriving newtype (Convex, Semigroup, Monoid, Show, Eq)

instance GHC.Exts.IsList (CD a) where
    type Item (CD a) = D a
    toList = unC . unCD
    fromList = CD . C

instance Functor CD where
    fmap f = CD . fmap (fmap f) . unCD

instance Applicative CD where
    pure = CD . pure . pure
    (CD xs) <*> (CD ys) = CD $ ((<*>) <$> xs) <*> ys

instance Foldable CD where
    foldMap f = foldMap (foldMap (f . fst) . toList) . unCD

instance Monad CD where
    (CD xs) >>= f = CD $ C $ unC xs >>= (toList . combine . fmap f)

instance Alternative CD where
    empty = CD mempty
    (CD xs) <|> (CD ys) = CD (xs <> ys)

instance MonadPlus CD where

newtype CSD a = CSD { unCSD :: C (SD a) } deriving newtype (Convex, Semigroup, Monoid, Show, Eq)

instance GHC.Exts.IsList (CSD a) where
    type Item (CSD a) = SD a
    toList = unC . unCSD
    fromList = CSD . C
