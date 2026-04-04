module BellKAT.Implementations.ProbAtomicOneStepQuantum
    ( ProbAtomicOneStepPolicy
    , ProbAtomicOneStepPolicy'
    , execute
    , execute'
    , executeWith
    , executeWith'
    ) where

import qualified GHC.Exts (IsList, Item)
import GHC.Exts (fromList, toList)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default
import Control.Subcategory.Functor

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Utils.Distribution (D', RationalOrDouble, DDom)
import qualified BellKAT.Utils.Distribution as D
import BellKAT.Utils.Convex
import BellKAT.Definitions.Core
import BellKAT.Definitions.Tests
import BellKAT.Definitions.Structures
import BellKAT.Definitions.Atomic
import BellKAT.Utils.Choice
import BellKAT.Implementations.Configuration
import BellKAT.Implementations.Output

-- | Essentially a symbol for `BellKAT.Utils.Automata.Guarded.GuardedFA` representing a set of
-- `ProbabilisticAtomicAction`s. In particular, it can be built from "basic actions" (`CreateBellPairArgs`). 
newtype ProbAtomicOneStepPolicy output tag = ProbAtomicOneStepPolicy (Set (ProbabilisticAtomicAction output tag))
    deriving newtype (Eq)
-- TODO: what's the difference between teh above design and the one in AtomicOneStepPolicy?

type ProbAtomicOneStepPolicy' tag = ProbAtomicOneStepPolicy (D' (TaggedBellPairs tag)) tag

instance (Show output, Tag tag, Default tag) => Show (ProbAtomicOneStepPolicy output tag) where
    show (ProbAtomicOneStepPolicy xs) = "{" <> intercalate "," (show <$> Set.toList xs) <> "}"

instance (Ord output, Ord tag) => GHC.Exts.IsList (ProbAtomicOneStepPolicy output tag) where
    type Item (ProbAtomicOneStepPolicy output tag) = ProbabilisticAtomicAction output tag
    fromList = ProbAtomicOneStepPolicy . GHC.Exts.fromList
    toList (ProbAtomicOneStepPolicy xs) = GHC.Exts.toList xs

instance (Monoid output, Ord output, Ord tag) => OrderedSemigroup (ProbAtomicOneStepPolicy output tag) where
    x <.> y = fromList $ (<.>) <$> toList x <*> toList y

instance (Monoid output, Ord output, Ord tag) => ParallelSemigroup (ProbAtomicOneStepPolicy output tag) where
    x <||> y = fromList $ (<||>) <$> toList x <*> toList y

instance (OpOutput output op, Monoid output, Ord output, STag output ~ tag, Ord tag)
        => CreatesBellPairs (ProbAtomicOneStepPolicy output tag) op tag where
    tryCreateBellPairFrom (CreateBellPairArgs i o p) = ProbAtomicOneStepPolicy $ Set.fromList $
            [ createProbabilitsticAtomicAction
                (createRestrictedTest mempty)
                (Mset.fromList' i)
                (fromCBPOutput (Mset.fromList' i) o p)
            ] <>
                if i /= mempty
                then [createProbabilitsticAtomicAction
                        (createRestrictedTest  [Mset.fromList' i])
                        mempty
                        mempty]
                else mempty


-- Interprets `ProbAtomicOneStepPolicy` as a monadic function from `TaggedBellPairs` to `CD'` of
-- `TaggedBellPairs`
execute
    :: (Output output, Ord (STag output))
    => Semigroup (CTag output)
    => (Tag (RTag output), Default (RTag output))
    => ProbAtomicOneStepPolicy output (STag output)
    -> OutputBellPairs output
    -> OutputM output (OutputBellPairs output)
execute (ProbAtomicOneStepPolicy xs) bps =
    foldMap (\paa -> executePAA id paa bps) xs

execute'
    :: (Output output, OutputM output ~ CD', RationalOrDouble p, Ord (STag output))
    => Semigroup (CTag output)
    => (DDom (RTag output), Default (RTag output))
    => ProbAtomicOneStepPolicy output (STag output)
    -> OutputBellPairs output 
    -> CD p (OutputBellPairs output)
execute' p bps = D.mapProbability fromRational $ execute p bps

executeWith
    :: (Output output, Ord (STag output))
    => Semigroup (CTag output)
    => (Tag (RTag output), Default (RTag output))
    => ExecutionParams (STag output) (RTag output) (CTag output)
    -> ProbAtomicOneStepPolicy output (STag output)
    -> OutputBellPairs output
    -> OutputM output (OutputBellPairs output)
executeWith ep (ProbAtomicOneStepPolicy xs) bps =
    foldMap (\paa -> executePAA (applyExecutionParams ep) paa bps) xs

executeWith'
    :: (Output output, OutputM output ~ CD', RationalOrDouble p, Ord (STag output))
    => Semigroup (CTag output)
    => (DDom (RTag output), Default (RTag output))
    => ExecutionParams (STag output) (RTag output) (CTag output)
    -> ProbAtomicOneStepPolicy output (STag output)
    -> OutputBellPairs output
    -> CD p (OutputBellPairs output)
executeWith' ep p bps = D.mapProbability fromRational $ executeWith ep p bps

executePAA :: (Output output, RuntimeTag (RTag output) tag) 
           => Ord tag
           => (Tag (RTag output), Default (RTag output))
           => Semigroup (CTag output)
           => (OutputBellPairs output -> OutputBellPairs output)
           -- ^ "fixing" function to apply at the end
           -> ProbabilisticAtomicAction output tag 
           -> OutputBellPairs output
           -> OutputM output (OutputBellPairs output)
executePAA fix act bps =
    let testHolds = (testBellPairs . paaTest) act (staticBellPairs bps)
     in if testHolds
        then mconcat
         [ cmap (fix . (<> rest)) (computeOutput (paaOutput act) chosen)
           | Partial { chosen , rest }  <- findElemsNDT (fmap staticTag) (toList . paaInputBPs $ act) bps]
        else mempty
