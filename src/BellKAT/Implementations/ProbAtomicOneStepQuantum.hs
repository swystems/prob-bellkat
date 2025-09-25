module BellKAT.Implementations.ProbAtomicOneStepQuantum
    ( ProbAtomicOneStepPolicy
    , ProbAtomicOneStepPolicy'
    , Output(..)
    , NetworkCapacity (NC)
    , execute
    , execute'
    , executeWithCapacity
    , executeWithCapacity'
    ) where

import qualified GHC.Exts (IsList, Item)
import GHC.Exts (fromList, toList)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default
import Control.Subcategory.Pointed
import Control.Subcategory.Functor
import Data.Typeable

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Utils.Distribution (D', RationalOrDouble)
import qualified BellKAT.Utils.Distribution as D
import BellKAT.Utils.Convex
import BellKAT.Definitions.Core
import BellKAT.Definitions.Tests
import BellKAT.Definitions.Structures
import BellKAT.Definitions.Atomic
import BellKAT.Utils.Choice

-- | Essentially a symbol for `BellKAT.Utils.Automata.Guarded.GuardedFA` representing a set of
-- `ProbabilisticAtomicAction`s. In particular, it can be built from "basic actions" (`CreateBellPairArgs`). 
newtype ProbAtomicOneStepPolicy output tag = ProbAtomicOneStepPolicy (Set (ProbabilisticAtomicAction output tag))
    deriving newtype (Eq)
-- TODO: what's the difference between teh above design and the one in AtomicOneStepPolicy?

type ProbAtomicOneStepPolicy' tag = ProbAtomicOneStepPolicy (D' (TaggedBellPairs tag)) tag

class Monoid output => Output output tag | output -> tag where
    computeOutput :: output -> TaggedBellPairs tag -> D' (TaggedBellPairs tag)
    fromProbabilisticBP :: TaggedBellPair tag -> Probability -> output

instance Ord tag => Output (D' (TaggedBellPairs tag)) tag where
    computeOutput x _ = x
    fromProbabilisticBP o p
      | p == 1 = cpure (Mset.singleton o)
      | p == 0 = cpure mempty
      | otherwise = D.choose p (Mset.singleton o) mempty

instance (Show output, Show tag, Ord tag, Default tag) => Show (ProbAtomicOneStepPolicy output tag) where
    show (ProbAtomicOneStepPolicy xs) = "{" <> intercalate "," (show <$> Set.toList xs) <> "}"

instance (Ord output, Ord tag) => GHC.Exts.IsList (ProbAtomicOneStepPolicy output tag) where
    type Item (ProbAtomicOneStepPolicy output tag) = ProbabilisticAtomicAction output tag
    fromList = ProbAtomicOneStepPolicy . GHC.Exts.fromList
    toList (ProbAtomicOneStepPolicy xs) = GHC.Exts.toList xs

instance (Monoid output, Ord output, Ord tag) => OrderedSemigroup (ProbAtomicOneStepPolicy output tag) where
    x <.> y = fromList $ (<.>) <$> toList x <*> toList y

instance (Monoid output, Ord output, Ord tag) => ParallelSemigroup (ProbAtomicOneStepPolicy output tag) where
    x <||> y = fromList $ (<||>) <$> toList x <*> toList y

instance (Output output tag, Ord output, Ord tag)
        => CreatesBellPairs (ProbAtomicOneStepPolicy output tag) tag where
    tryCreateBellPairFrom (CreateBellPairArgs i o p _) = ProbAtomicOneStepPolicy $ Set.fromList $
            [ createProbabilitsticAtomicAction
                (createRestrictedTest mempty)
                (Mset.fromList i)
                (fromProbabilisticBP o p)
            ] <>
                if i /= mempty
                then [createProbabilitsticAtomicAction
                        (createRestrictedTest  [Mset.fromList i])
                        mempty
                        mempty]
                else mempty

-- | Network capacity, i.e., the maximum number of each possible `BellPair`, essentially
-- a `TaggedBellPairs`
newtype NetworkCapacity tag = NC 
    { unNC :: TaggedBellPairs tag 
    } deriving newtype (Monoid, Semigroup)
    -- TODO: should really be BellPairs

instance Ord tag => GHC.Exts.IsList (NetworkCapacity tag) where
    type Item (NetworkCapacity tag) = TaggedBellPair tag
    fromList = NC . fromList
    toList = toList . unNC

-- Interprets `ProbAtomicOneStepPolicy` as a monadic function from `TaggedBellPairs` to `CD'` of
-- `TaggedBellPairs`
execute
    :: (Output output tag, Typeable tag, Show tag, Default tag, Ord tag)
    => ProbAtomicOneStepPolicy output tag -> TaggedBellPairs tag -> CD' (TaggedBellPairs tag)
execute (ProbAtomicOneStepPolicy xs) bps =
    foldMap (\paa -> executePAA id paa bps) xs

execute'
    :: (Output output tag, Typeable tag, RationalOrDouble p, Show tag, Default tag, Ord tag)
    => ProbAtomicOneStepPolicy output tag -> TaggedBellPairs tag -> CD p (TaggedBellPairs tag)
execute' p bps = D.mapProbability fromRational $ execute p bps

executeWithCapacity
    :: (Output output tag, Typeable tag, Show tag, Default tag, Ord tag)
    => NetworkCapacity tag
    -> ProbAtomicOneStepPolicy output tag
    -> TaggedBellPairs tag -> CD' (TaggedBellPairs tag)
executeWithCapacity nc (ProbAtomicOneStepPolicy xs) bps =
    foldMap (\paa -> executePAA (fixNetworkCapacity nc) paa bps) xs

executeWithCapacity'
    :: (Output output tag, Typeable tag, RationalOrDouble p, Show tag, Default tag, Ord tag)
    => NetworkCapacity tag -> ProbAtomicOneStepPolicy output tag
    -> TaggedBellPairs tag -> CD p (TaggedBellPairs tag)
executeWithCapacity' nc p bps = D.mapProbability fromRational $ executeWithCapacity nc p bps

executePAA :: (Output output tag, Show tag, Ord tag, Default tag, Typeable tag)
           => (TaggedBellPairs tag -> TaggedBellPairs tag)
           -- ^ "fixing" function to apply at the end
           -> ProbabilisticAtomicAction output tag -> TaggedBellPairs tag -> CD' (TaggedBellPairs tag)
executePAA fix act bps =
    if (getBPsPredicate . toBPsPredicate . paaTest) act bps
       then fromList
         [ cmap (fix . (<> rest)) (computeOutput (paaOutput act) chosen)
           | Partial { chosen , rest }  <- findElemsND (toList . paaInputBPs $ act) bps]
       else mempty

fixNetworkCapacity :: Ord tag => NetworkCapacity tag -> TaggedBellPairs tag -> TaggedBellPairs tag
fixNetworkCapacity (NC x) = Mset.min x
