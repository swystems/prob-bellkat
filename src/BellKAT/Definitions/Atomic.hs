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
    ProbabilisticAtomicAction(..),
    createProbabilitsticAtomicAction,
    -- * Re-export `RestrictedTest`s
    RestrictedTest,
    createRestrictedTest,
    (.+.),
    (.&&.),
) where

import Data.Default
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

data ProbabilisticAtomicAction tag = ProbabilisticAtomicAction
    { paaTest :: RestrictedTest ()
    , paaIO :: [(TaggedBellPairs (), Output tag)]
    } deriving stock (Eq, Ord)

-- | Creates `AtomicAction` normalizing `TaggedBellPairs` components of "zero" atomic actions
createProbabilitsticAtomicAction ::
    Ord tag =>
    RestrictedTest () ->
    [(TaggedBellPairs (), Output tag)] ->
    ProbabilisticAtomicAction tag
createProbabilitsticAtomicAction t io =
    if t == createRestrictedTest [mempty]
        then ProbabilisticAtomicAction t mempty 
        else ProbabilisticAtomicAction t io

instance Ord tag => OrderedSemigroup (ProbabilisticAtomicAction tag) where
    (ProbabilisticAtomicAction t1 io1) <.> (ProbabilisticAtomicAction t2 io2) =
        createProbabilitsticAtomicAction
            (t1 .&&. (t2 .+. inBps1))
            (io1 <> io2)
        where
            inBps1 = mconcat [inBPs | (inBPs, _) <- io1]

instance Ord tag => ParallelSemigroup (ProbabilisticAtomicAction tag) where
    (ProbabilisticAtomicAction t1 io1) <||> (ProbabilisticAtomicAction t2 io2) =
        createProbabilitsticAtomicAction
            ((t1 .+. inBps2) .&&. (t2 .+. inBps1))
            (io1 <> io2)
        where
            inBps1 = mconcat [inBPs | (inBPs, _) <- io1]
            inBps2 = mconcat [inBPs | (inBPs, _) <- io2]

instance (Show tag, Default tag, Eq tag, Ord tag) => Show (ProbabilisticAtomicAction tag) where
    showsPrec _ (ProbabilisticAtomicAction t io) =
            showString "["
            . shows t
            . showString "]"
            . showString ""
            . showString "▶"
            . shows io
            . showString ""
