module BellKAT.Utils.Automata.HyperAction 
    (HyperAction) where

import           Data.Pointed
import qualified Data.Set                     as Set
import           Data.Set                     (Set)
import           Data.Foldable (toList)

import           BellKAT.Definitions.Structures.Basic

newtype HyperAction a = HyperAction (Set a)
    deriving newtype (Foldable, Pointed)

instance Ord a => ChoiceSemigroup (HyperAction a) where
    (HyperAction a) <+> (HyperAction b) = HyperAction (a <> b)

instance (Ord a, OrderedSemigroup a) => OrderedSemigroup (HyperAction a) where
    (HyperAction xs) <.> (HyperAction ys) = HyperAction $
        Set.fromList [x <.> y | x <- toList xs, y <- toList ys ]

instance (Ord a, ParallelSemigroup a) => ParallelSemigroup (HyperAction a) where
    (HyperAction xs) <||> (HyperAction ys) = HyperAction $
        Set.fromList [x <||> y | x <- toList xs, y <- toList ys ]
