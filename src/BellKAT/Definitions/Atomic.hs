{- |
   Module : BellKAT.Definitions.Atomic
   Description : Syntactic definitions related to atomic actions

   Only contains syncatic structure of atomic actions, behavior is defined elsewhere (e.g., see
   "BellKAT.Implementations.AtomicOneStepHistoryQuantum").
-}
module BellKAT.Definitions.Atomic (
    -- * Restricted tests
    RestrictedTest,
    createRestrictedTest,
    (.+.),
    (.&&.),

    -- * Atomic actions
    AtomicAction,
    aaTest,
    aaInputBPs,
    aaOutputBPs,
    createAtomicAction,
    ProbabilisticAtomicAction,
    createProbabilitsticAtomicAction,
    paaTest,
    paaInputBPs,
    paaOutputBPD,
) where

import Data.Default
import Data.Foldable (toList)
import Data.List
import qualified Data.Multiset as Mset

import qualified Numeric.Probability.Distribution as P

import BellKAT.Definitions.Core
import BellKAT.Definitions.Structures

-- | Represents tests as a set of `TaggedBellPairs` that /each/ must not appear in the input
newtype RestrictedTest tag = RestrictedTest [TaggedBellPairs tag]
    deriving newtype (Eq, Ord)

-- | Constructs `RestrictedTest` in a normalized (non-redundant) form
createRestrictedTest :: Ord tag => [TaggedBellPairs tag] -> RestrictedTest tag
createRestrictedTest = RestrictedTest . sort . restrictedTestNormalize

restrictedTestNormalize :: Ord tag => [TaggedBellPairs tag] -> [TaggedBellPairs tag]
restrictedTestNormalize [] = []
restrictedTestNormalize (x : xs) =
    if any (`Mset.isSubsetOf` x) xs
        then restrictedTestNormalize xs
        else x : restrictedTestNormalize xs

instance (Show tag, Default tag, Eq tag) => Show (RestrictedTest tag) where
    showsPrec _ (RestrictedTest []) = showString "TRUE"
    showsPrec _ (RestrictedTest (x : xs)) = shows (toList x) . showsRest xs
      where
        showsRest [] = id
        showsRest (y : ys) = showString " /\\ " . shows (toList y) . showsRest ys

-- | Performs multiset union of each set in a `RestrictedTest` with the given `TaggedBellPairs`
(.+.) :: (Ord tag) => RestrictedTest tag -> TaggedBellPairs tag -> RestrictedTest tag
(RestrictedTest bpss) .+. bps = createRestrictedTest (map (<> bps) bpss)

-- | Performs logical /and/ of two `RestrictedTest`s
(.&&.) :: (Ord tag) => RestrictedTest tag -> RestrictedTest tag -> RestrictedTest tag
(RestrictedTest bpss) .&&. (RestrictedTest bpss') =
    createRestrictedTest (bpss <> bpss')

instance Test RestrictedTest where
    toBPsPredicate (RestrictedTest s) = BPsPredicate $ \bps -> not (any (`Mset.isSubsetOf` bps) s)

data AtomicAction tag = AtomicAction
    { aaTest :: RestrictedTest tag
    , aaInputBPs :: TaggedBellPairs tag
    , aaOutputBPs :: TaggedBellPairs tag
    }
    deriving stock (Eq, Ord)

instance (Show tag, Default tag, Eq tag) => Show (AtomicAction tag) where
    showsPrec _ (AtomicAction t inBPs outBPs) =
        showString "["
            . shows t
            . showString "] ("
            . shows (toList inBPs)
            . showString "|>"
            . shows (toList outBPs)
            . showString ")"

instance Ord tag => OrderedSemigroup (AtomicAction tag) where
    (AtomicAction t1 inBps1 outBps1) <.> (AtomicAction t2 inBps2 outBps2) =
        createAtomicAction (t1 .&&. (t2 .+. inBps1)) (inBps1 <> inBps2) (outBps1 <> outBps2)

instance Ord tag => ParallelSemigroup (AtomicAction tag) where
    (AtomicAction t1 inBps1 outBps1) <||> (AtomicAction t2 inBps2 outBps2) =
        createAtomicAction ((t1 .+. inBps2) .&&. (t2 .+. inBps1)) (inBps1 <> inBps2) (outBps1 <> outBps2)

-- | Creates `AtomicAction` normalizing `TaggedBellPairs` components of "zero" atomic actions
createAtomicAction ::
    Ord tag =>
    RestrictedTest tag ->
    TaggedBellPairs tag ->
    TaggedBellPairs tag ->
    AtomicAction tag
createAtomicAction t inBPs outBPs =
    if t == createRestrictedTest [mempty]
        then AtomicAction t mempty mempty
        else AtomicAction t inBPs outBPs

type Dist = P.T Probability

data ProbabilisticAtomicAction tag = ProbabilisticAtomicAction
    { paaTest :: RestrictedTest tag
    , paaInputBPs :: TaggedBellPairs tag
    , paaOutputBPD :: Dist (TaggedBellPairs tag)
    }

instance (Ord tag) => Eq (ProbabilisticAtomicAction tag) where
    (ProbabilisticAtomicAction t i o) == (ProbabilisticAtomicAction t' i' o') =
        t == t' && i == i' && P.equal o o'

-- | Creates `AtomicAction` normalizing `TaggedBellPairs` components of "zero" atomic actions
createProbabilitsticAtomicAction ::
    Ord tag =>
    RestrictedTest tag ->
    TaggedBellPairs tag ->
    Dist (TaggedBellPairs tag) ->
    ProbabilisticAtomicAction tag
createProbabilitsticAtomicAction t inBPs outBPs =
    if t == createRestrictedTest [mempty]
        then ProbabilisticAtomicAction t mempty (pure mempty)
        else ProbabilisticAtomicAction t inBPs outBPs

instance Ord tag => OrderedSemigroup (ProbabilisticAtomicAction tag) where
    (ProbabilisticAtomicAction t1 inBps1 outBpsD1) <.> (ProbabilisticAtomicAction t2 inBps2 outBpsD2) =
        createProbabilitsticAtomicAction
            (t1 .&&. (t2 .+. inBps1))
            (inBps1 <> inBps2)
            ((<>) <$> outBpsD1 <*> outBpsD2)

instance Ord tag => ParallelSemigroup (ProbabilisticAtomicAction tag) where
    (ProbabilisticAtomicAction t1 inBps1 outBps1) <||> (ProbabilisticAtomicAction t2 inBps2 outBps2) =
        createProbabilitsticAtomicAction
            ((t1 .+. inBps2) .&&. (t2 .+. inBps1))
            (inBps1 <> inBps2)
            ((<>) <$> outBps1 <*> outBps2)

instance (Show tag, Default tag, Eq tag, Ord tag) => Show (ProbabilisticAtomicAction tag) where
    showsPrec _ (ProbabilisticAtomicAction t inBPs outBPs) =
        showString "["
            . shows t
            . showString "] ("
            . shows (toList inBPs)
            . showString "|>"
            . showString (P.pretty show outBPs)
            . showString ")"
