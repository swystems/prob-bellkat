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
    Output(..),
    asFunction,
    -- * Re-export `RestrictedTest`s
    RestrictedTest,
    createRestrictedTest,
    (.+.),
    (.&&.),
    ValidTag
) where

import Data.Default
import Control.Subcategory.Functor
import Control.Subcategory.Pointed

import qualified BellKAT.Utils.Multiset as Mset
import BellKAT.Utils.Distribution as D
import BellKAT.Definitions.Core
import BellKAT.Definitions.Tests
import BellKAT.Definitions.Structures
import BellKAT.Implementations.QuantumOps

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

data Output =
    Skip
    -- ^ yiels mempty
    | Try D.Probability (TaggedBellPair QuantumTag)
    -- ^ yields a (trivial) probabilistic choice: singleton over the given TBP or empty
    | Swap D.Probability (TaggedBellPair QuantumTag)
    -- ^ yields the swapped Bell pair given the two in input, with probability p
    | Distill (TaggedBellPair QuantumTag)
    -- ^ yields the distilled Bell pair given the two same-location ones in input
    -- ^ computing the probability dynamically
    deriving stock (Eq, Ord, Show)

-- | Ensure that any tag type used with the Output abstraction
-- | can be interpreted into a distribution D'
class ValidTag tag where
    asFunction :: Output -> TaggedBellPairs tag -> D' (TaggedBellPairs tag)

-- | Define an interpretation yeilding quantitatively correct results
-- | TODO: Should this be in QuantumOps.hs ?
-- | Pavel: Yes, I think it should be possible to keep this particular module tag agnostic. You can define a class instance in the module where you define the quantum tag.
instance ValidTag QuantumTag where
    asFunction Skip _ =
        cpure mempty

    asFunction (Try p o) _
            | p == 0 = cpure mempty
            | p == 1 = cpure (Mset.singleton o)
            | otherwise = D.choose p (Mset.singleton o) mempty

    asFunction (Swap p o) chosenBPs =
        swapBPs p chosenBPs o

    asFunction (Distill o) chosenBPs =
        distBPs chosenBPs o

-- | Workaround for the Maybe QuantumTag type - the type we set in `Prelude.hs`
instance (ValidTag t, Ord t, Default t) => ValidTag (Maybe t) where
    asFunction out bps =
        let bps' = Mset.map (\(TaggedBellPair bp mtag) -> TaggedBellPair bp (maybe def id mtag)) bps
            resultQ = asFunction out bps'
            result = cmap (Mset.map (\(TaggedBellPair bp tag) -> TaggedBellPair bp (Just tag))) resultQ
        in result

-- | Defer the probabilistic branching to the execution phase (via asFunction):
-- | Match inputs by location only, disregarding the tag,
-- | which is only relevant for the `Output` function (probabilistic interpretation)
data ProbabilisticAtomicAction tag = ProbabilisticAtomicAction
    { paaTest :: RestrictedTest ()
    , paaIO :: [(TaggedBellPairs (), Output)]
    } deriving stock (Eq, Ord)

-- | Creates `AtomicAction` normalizing `TaggedBellPairs` components of "zero" atomic actions
createProbabilitsticAtomicAction ::
    Ord tag =>
    RestrictedTest () ->
    [(TaggedBellPairs (), Output)] ->
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
