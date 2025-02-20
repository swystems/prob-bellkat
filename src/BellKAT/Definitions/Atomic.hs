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
import Control.Subcategory.Functor
import Control.Subcategory.Pointed
import Control.Subcategory.Applicative

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

data ProbabilisticAtomicAction tag = ProbabilisticAtomicAction
    { paaTest :: RestrictedTest tag
    , paaInputBPs :: TaggedBellPairs tag
    , paaOutputBPD :: D' (TaggedBellPairs tag)
    } deriving stock (Eq, Ord)

-- | Creates `AtomicAction` normalizing `TaggedBellPairs` components of "zero" atomic actions
createProbabilitsticAtomicAction ::
    Ord tag =>
    RestrictedTest tag ->
    TaggedBellPairs tag ->
    D' (TaggedBellPairs tag) ->
    ProbabilisticAtomicAction tag
createProbabilitsticAtomicAction t inBPs outBPs =
    if t == createRestrictedTest [mempty]
        then ProbabilisticAtomicAction t mempty (cpure mempty)
        else ProbabilisticAtomicAction t inBPs outBPs

instance Ord tag => OrderedSemigroup (ProbabilisticAtomicAction tag) where
    (ProbabilisticAtomicAction t1 inBps1 outBpsD1) <.> (ProbabilisticAtomicAction t2 inBps2 outBpsD2) =
        createProbabilitsticAtomicAction
            (t1 .&&. (t2 .+. inBps1))
            (inBps1 <> inBps2)
            (cmap (uncurry (<>)) $ pair outBpsD1 outBpsD2)

instance Ord tag => ParallelSemigroup (ProbabilisticAtomicAction tag) where
    (ProbabilisticAtomicAction t1 inBps1 outBps1) <||> (ProbabilisticAtomicAction t2 inBps2 outBps2) =
        createProbabilitsticAtomicAction
            ((t1 .+. inBps2) .&&. (t2 .+. inBps1))
            (inBps1 <> inBps2)
            (cmap (uncurry (<>)) $ pair outBps1 outBps2)

instance (Show tag, Default tag, Eq tag, Ord tag) => Show (ProbabilisticAtomicAction tag) where
    showsPrec _ (ProbabilisticAtomicAction t inBPs outBPs) =
            shows t
            . showString " "
            . shows inBPs
            . showString "|>"
            . shows outBPs
            . showString ""
