module BellKAT.Implementations.ProbAtomicOneStepQuantum
    ( ProbAtomicOneStepPolicy
    ) where

import Data.Foldable (toList)
import qualified GHC.Exts (IsList, Item, fromList, toList) 
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Utils.Distribution as D
import BellKAT.Definitions.Core
import BellKAT.Definitions.Structures
import BellKAT.Definitions.Atomic

-- TODO: what's the difference between this design and the one in AtomicOneStepPolicy?
newtype ProbAtomicOneStepPolicy tag = ProbAtomicOneStepPolicy (Set (ProbabilisticAtomicAction tag))
    deriving newtype (Eq)

instance (Show tag, Ord tag, Default tag) => Show (ProbAtomicOneStepPolicy tag) where
    show (ProbAtomicOneStepPolicy xs) = show $ Set.toList xs

instance Ord tag => GHC.Exts.IsList (ProbAtomicOneStepPolicy tag) where
    type Item (ProbAtomicOneStepPolicy tag) = ProbabilisticAtomicAction tag
    fromList = ProbAtomicOneStepPolicy . GHC.Exts.fromList
    toList (ProbAtomicOneStepPolicy xs) = GHC.Exts.toList xs

instance Ord tag => OrderedSemigroup (ProbAtomicOneStepPolicy tag) where
    (ProbAtomicOneStepPolicy xs) <.> (ProbAtomicOneStepPolicy ys) = 
        ProbAtomicOneStepPolicy $ Set.fromList $ (<.>) <$> toList xs <*> toList ys

instance Ord tag => ParallelSemigroup (ProbAtomicOneStepPolicy tag) where
    (ProbAtomicOneStepPolicy xs) <||> (ProbAtomicOneStepPolicy ys) = 
        ProbAtomicOneStepPolicy $ Set.fromList $ (<||>) <$> toList xs <*> toList ys

instance Ord tag => CreatesBellPairs (ProbAtomicOneStepPolicy tag) tag where
    tryCreateBellPairFrom (CreateBellPairArgs o i p _) = ProbAtomicOneStepPolicy $ Set.fromList $
            [ createProbabilitsticAtomicAction 
                (createRestrictedTest mempty) 
                (Mset.fromList i) 
                (if p == 1 then pure (Mset.singleton o) else D.choose p (Mset.singleton o) mempty)
            ] <> 
                if i /= mempty 
                then [createProbabilitsticAtomicAction 
                        (createRestrictedTest  [Mset.fromList i]) 
                        mempty 
                        (pure mempty)]
                else mempty
