{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module BellKAT.Utils.Cost (
    CostCD(..),
    CostCD'
) where

import Control.Subcategory.Pointed
import Control.Subcategory.Functor
import Control.Subcategory.Bind
import Data.Monoid
import Control.Arrow
import BellKAT.Utils.Convex
import BellKAT.Utils.Convex.Constraint
import BellKAT.Utils.Distribution

-- | Cost wrapper, specialized to operate over CD p distributions, storing an integer cost along with the value.
newtype CostCD p a = CostCD { unCostCD :: CD p (a, Sum Int) }
    deriving newtype (Show, Eq, Ord, Semigroup, Monoid)

instance Foldable (CostCD p) where -- Foldable (CD p) is implied.
    foldMap f = foldMap (f . fst) . unCostCD

instance Constrained (CostCD p) where
    type Dom (CostCD p) a = (DDom a, DDom (a, Sum Int))

instance RationalOrDouble p => CPointed (CostCD p) where
    cpure = CostCD . cpure . (,mempty)

instance RationalOrDouble p => CFunctor (CostCD p) where
    cmap f = CostCD . cmap (first f) . unCostCD

instance (RationalOrDouble p) => CBind (CostCD p) where
    cjoin = CostCD . cjoin . cmap (\(CostCD ma, c) -> cmap (second (c <>)) ma) . unCostCD

type CostCD' = CostCD Probability
