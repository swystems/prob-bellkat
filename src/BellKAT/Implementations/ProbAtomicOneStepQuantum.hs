module BellKAT.Implementations.ProbAtomicOneStepQuantum
    ( ProbAtomicOneStepPolicy
    , execute
    ) where

import Data.Foldable (toList)
import qualified GHC.Exts (IsList, Item, toList) 
import GHC.Exts (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default
import Control.Applicative
import BellKAT.Utils.Choice

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Utils.Distribution as D
import BellKAT.Utils.ConvexSetOfDistributions
import BellKAT.Definitions.Core
import BellKAT.Definitions.Tests
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

execute :: Ord tag => ProbAtomicOneStepPolicy tag -> TaggedBellPairs tag -> CD (TaggedBellPairs tag)
execute (ProbAtomicOneStepPolicy xs) bps = foldMap (`executePAA` bps) xs

executePAA :: Ord tag 
           => ProbabilisticAtomicAction tag -> TaggedBellPairs tag -> CD (TaggedBellPairs tag)
executePAA act bps = 
    if (getBPsPredicate . toBPsPredicate . paaTest) act bps 
       then fromList [ (<> rest partial) <$> paaOutputBPD act | partial <- findElemsND (toList . paaInputBPs $ act) bps]
       else empty



