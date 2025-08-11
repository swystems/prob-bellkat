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
    createProbabilitsticAtomicAction,
    paaTest,
    paaInputBPs,
    paaOutputBPD,
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
import Control.Subcategory.Applicative

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

-- | Possible function that can be applied to a MS of tagged Bell pairs
-- | to produce a distribution over a new MS of tagged Bell pair 
data Output = 
    NoOp 
    -- ^ yields empty set
    | Do (TaggedBellPair QuantumTag) 
    -- ^ yields a singleton distribution (dirac) over the given TBP 
    | Try D.Probability (TaggedBellPair QuantumTag) 
    -- ^ yields a (trivial) probabilistic choice: singleton over the given TBP or empty
    | Swap D.Probability
    -- ^ yields the swapped Bell pair given the two in input, with probability p
    | Distill
    -- ^ yields the distilled Bell pair given the two same-location ones in input
    -- ^ computing the probability dynamically
    | Merge Output Output
    -- ^ yields a merged distribution of outputs
    deriving stock (Eq, Ord, Show)

-- | Ensure that any tag type used with the Output abstraction
-- | can be interpreted into a distribution D'
class ValidTag tag where
    asFunction :: Output -> TaggedBellPairs tag -> D' (TaggedBellPairs tag)

-- | Define an interpretation yeilding quantitatively correct results
-- | TODO: Should this be in QuantumOps.hs ?
instance ValidTag QuantumTag where
    asFunction NoOp _ = cpure mempty
    asFunction (Do o) _ = cpure (Mset.singleton o)
    asFunction (Try p o) _ = D.choose p (Mset.singleton o) mempty
    asFunction (Swap p) chosenBPs = swapBPs p chosenBPs
    asFunction Distill chosenBPs = distBPs chosenBPs
    asFunction (Merge o1 o2) chosenBPs =
        -- given the two ^  ^ functions yielding resp. outBps1 outBps2
        let outBps1 = asFunction o1 chosenBPs
            outBps2 = asFunction o2 chosenBPs
        -- define their composite output as:
        in cmap (uncurry (<>)) $ pair outBps1 outBps2
        -- ^ EXTENSION: here I should define how the multiset timestamp evolves

-- | TODO: here there is probably something that does not make sense:
-- | for Do and Try, modeling cr,tr,gen, we infer the `o` output TBP from the abstract semantics
-- | whereas for Swap and Distill we infer it when interpreting given the input TBP, 
-- | which I guess is not necessary if we'd pass the `o` output TBP

-- | Workaround for the Maybe QuantumTag type - the type we set in `Prelude.hs`
-- | TODO: is there any way to avoid this? 
-- | is there a way to set QuantumTag and not Maybe QuantumTag?
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
    , paaInputBPs :: TaggedBellPairs ()
    , paaOutputBPD :: Output
    } deriving stock (Eq, Ord)

-- | Creates `AtomicAction` normalizing `TaggedBellPairs` components of "zero" atomic actions
-- TYPO: Probabilitstic
createProbabilitsticAtomicAction ::
    Ord tag =>
    RestrictedTest () ->
    TaggedBellPairs () ->
    Output ->
    ProbabilisticAtomicAction tag
createProbabilitsticAtomicAction t inBPs outBPs =
    if t == createRestrictedTest [mempty]
        then ProbabilisticAtomicAction t mempty NoOp
        else ProbabilisticAtomicAction t inBPs outBPs

instance Ord tag => OrderedSemigroup (ProbabilisticAtomicAction tag) where
    (ProbabilisticAtomicAction t1 inBps1 o1) <.> (ProbabilisticAtomicAction t2 inBps2 o2) =
        createProbabilitsticAtomicAction
            (t1 .&&. (t2 .+. inBps1))
            (inBps1 <> inBps2)
            (Merge o1 o2)

instance Ord tag => ParallelSemigroup (ProbabilisticAtomicAction tag) where
    (ProbabilisticAtomicAction t1 inBps1 o1) <||> (ProbabilisticAtomicAction t2 inBps2 o2) =
        createProbabilitsticAtomicAction
            ((t1 .+. inBps2) .&&. (t2 .+. inBps1))
            (inBps1 <> inBps2)
            (Merge o1 o2)

instance (Show tag, Default tag, Eq tag, Ord tag) => Show (ProbabilisticAtomicAction tag) where
    showsPrec _ (ProbabilisticAtomicAction t inBPs outBPs) =
            showString "["
            . shows t
            . showString "]"
            . showString ""
            . shows inBPs
            . showString "▶"
            . shows outBPs
            . showString ""
