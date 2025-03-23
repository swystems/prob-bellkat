module BellKAT.Implementations.ProbAtomicOneStepQuantum
    ( ProbAtomicOneStepPolicy
    , NetworkCapacity (NC)
    , execute
    , execute'
    , executeWithCapacity
    , executeWithCapacity'
    ) where

import qualified GHC.Exts (IsList, Item) 
import GHC.Exts (fromList, toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default
import Control.Subcategory.Pointed
import Control.Subcategory.Functor
import Data.Typeable

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Utils.Distribution as D
import BellKAT.Utils.Convex
import BellKAT.Definitions.Core
import BellKAT.Definitions.Tests
import BellKAT.Definitions.Structures
import BellKAT.Definitions.Atomic
import BellKAT.Utils.Choice

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
    x <.> y = fromList $ (<.>) <$> toList x <*> toList y

instance Ord tag => ParallelSemigroup (ProbAtomicOneStepPolicy tag) where
    x <||> y = fromList $ (<||>) <$> toList x <*> toList y

instance Ord tag => CreatesBellPairs (ProbAtomicOneStepPolicy tag) tag where
    tryCreateBellPairFrom (CreateBellPairArgs i o p _) = ProbAtomicOneStepPolicy $ Set.fromList $
            [ createProbabilitsticAtomicAction 
                (createRestrictedTest mempty) 
                (Mset.fromList i) 
                (if p == 1 
                 then cpure (Mset.singleton o) 
                 else if p == 0
                 then cpure mempty
                 else D.choose p (Mset.singleton o) mempty)
            ] <> 
                if i /= mempty 
                then [createProbabilitsticAtomicAction 
                        (createRestrictedTest  [Mset.fromList i]) 
                        mempty 
                        (cpure mempty)]
                else mempty

newtype NetworkCapacity tag = NC 
    { unNC :: TaggedBellPairs tag 
    } deriving newtype (Monoid, Semigroup)
    -- TODO: should really be BellPairs

instance Ord tag => GHC.Exts.IsList (NetworkCapacity tag) where
    type Item (NetworkCapacity tag) = TaggedBellPair tag
    fromList = NC . fromList
    toList = toList . unNC

execute :: (Typeable tag, Show tag, Default tag, Ord tag) => ProbAtomicOneStepPolicy tag -> TaggedBellPairs tag -> CD' (TaggedBellPairs tag)
execute (ProbAtomicOneStepPolicy xs) bps = 
    foldMap (\paa -> executePAA id paa bps) xs

execute' :: (Typeable tag, RationalOrDouble p, Show tag, Default tag, Ord tag) => ProbAtomicOneStepPolicy tag -> TaggedBellPairs tag -> CD p (TaggedBellPairs tag)
execute' p bps = mapProbability fromRational $ execute p bps

executeWithCapacity :: (Typeable tag, Show tag, Default tag, Ord tag) => NetworkCapacity tag -> ProbAtomicOneStepPolicy tag -> TaggedBellPairs tag -> CD' (TaggedBellPairs tag)
executeWithCapacity nc (ProbAtomicOneStepPolicy xs) bps = 
    foldMap (\paa -> executePAA (fixNetworkCapacity nc) paa bps) xs

executeWithCapacity' :: (Typeable tag, RationalOrDouble p, Show tag, Default tag, Ord tag) => NetworkCapacity tag -> ProbAtomicOneStepPolicy tag -> TaggedBellPairs tag -> CD p (TaggedBellPairs tag)
executeWithCapacity' nc p bps = mapProbability fromRational $ executeWithCapacity nc p bps

executePAA :: (Show tag, Ord tag, Default tag, Typeable tag)
           => (TaggedBellPairs tag -> TaggedBellPairs tag) 
           -> ProbabilisticAtomicAction tag -> TaggedBellPairs tag -> CD' (TaggedBellPairs tag)
executePAA fix act bps = 
    if (getBPsPredicate . toBPsPredicate . paaTest) act bps 
       then fromList [ cmap (fix . (<> rest partial)) (paaOutputBPD act) | partial <- findElemsND (toList . paaInputBPs $ act) bps]
       else mempty

fixNetworkCapacity :: Ord tag => NetworkCapacity tag -> TaggedBellPairs tag -> TaggedBellPairs tag
fixNetworkCapacity (NC x) = Mset.min x
