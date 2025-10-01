{- |
   Module : BellKAT.Definitions.Atomic
   Description : Syntactic definitions related to atomic actions

   Only contains syncatic structure of atomic actions, behavior is defined elsewhere (e.g., see
   "BellKAT.Implementations.AtomicOneStepHistoryQuantum").
-}
module BellKAT.Definitions.Atomic (
    -- * Atomic actions
    AtomicAction,
    aaTest,
    aaInputBPs,
    aaOutputBPs,
    createAtomicAction,
    ProbabilisticAtomicAction,
    ProbabilisticAtomicAction',
    createProbabilitsticAtomicAction,
    paaTest,
    paaInputBPs,
    paaOutput,
    -- * Re-export `RestrictedTest`s
    RestrictedTest,
    createRestrictedTest,
    (.+.),
    (.&&.),
) where

import Data.Default

import BellKAT.Utils.Distribution as D
import BellKAT.Definitions.Core
import BellKAT.Definitions.Tests
import BellKAT.Definitions.Structures

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
            . showString "]("
            . shows inBPs
            . showString "▶"
            . shows outBPs
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

data ProbabilisticAtomicAction output tag = ProbabilisticAtomicAction
    { paaTest :: RestrictedTest tag
    , paaInputBPs :: TaggedBellPairs tag
    , paaOutput :: output
    } deriving stock (Eq, Ord)

type ProbabilisticAtomicAction' tag = ProbabilisticAtomicAction (D' (TaggedBellPairs tag)) tag

-- | Creates `AtomicAction` normalizing `TaggedBellPairs` components of "zero" atomic actions
createProbabilitsticAtomicAction ::
    (Monoid output, Ord tag) =>
    RestrictedTest tag ->
    TaggedBellPairs tag ->
    output ->
    ProbabilisticAtomicAction output tag
createProbabilitsticAtomicAction t inBPs o =
    if t == createRestrictedTest [mempty]
        then ProbabilisticAtomicAction t mempty mempty
        else ProbabilisticAtomicAction t inBPs o

instance (Monoid output, Ord tag) => OrderedSemigroup (ProbabilisticAtomicAction output tag) where
    (ProbabilisticAtomicAction t1 inBps1 out1) <.> (ProbabilisticAtomicAction t2 inBps2 out2) =
        createProbabilitsticAtomicAction
            (t1 .&&. (t2 .+. inBps1))
            (inBps1 <> inBps2)
            (out1 <> out2)

instance (Monoid output, Ord tag) => ParallelSemigroup (ProbabilisticAtomicAction output tag) where
    (ProbabilisticAtomicAction t1 inBps1 out1) <||> (ProbabilisticAtomicAction t2 inBps2 out2) =
        createProbabilitsticAtomicAction
            ((t1 .+. inBps2) .&&. (t2 .+. inBps1))
            (inBps1 <> inBps2)
            (out1 <> out2)

instance (Show output, Show tag, Default tag, Eq tag, Ord tag) => Show (ProbabilisticAtomicAction output tag) where
    showsPrec _ (ProbabilisticAtomicAction t inBPs out) =
            showString "["
            . shows t
            . showString "]"
            . shows inBPs
            . showString "▶"
            . shows out
            . showString ""
