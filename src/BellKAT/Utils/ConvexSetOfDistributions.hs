{-# LANGUAGE TupleSections #-}
module BellKAT.Utils.ConvexSetOfDistributions 
    ( C
    , CD
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.List
import qualified GHC.Exts (IsList, Item)
import           GHC.Exts (fromList, toList)

import           BellKAT.Utils.Distribution as D

class Convex a where
    combine :: D a -> a

instance Convex (D a) where
    combine = D.join

newtype C a = C { unC :: [a] } 
    deriving newtype (Functor, Applicative, Alternative, Monad, Foldable, MonadPlus,
                                     Semigroup, Monoid, Eq)

instance Show a => Show (C a) where
    show (C xs) = "(|" <> intercalate "," (show <$> xs) <> "|)"

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
    (CD xs) >>= f = CD $ xs >>= (unCD . combine . fmap f)

instance Alternative CD where
    empty = CD empty
    (CD xs) <|> (CD ys) = CD (xs <|> ys)

instance MonadPlus CD where
