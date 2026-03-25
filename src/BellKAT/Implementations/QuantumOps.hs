{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{- |
   Module : BellKAT.Implementations.QuantumOps
   Description : Syntactic definitions related to quantum operations
-}
module BellKAT.Implementations.QuantumOps (
    -- * Quantum tags
    QuantumTag(..),
    QuantumOutput(..),
    MaxClock(..),
    SpaceUnit,
    TimeUnit,
    Werner,
    -- * Primitive quantum operations (exported for testing)
    swapBPs,
    createBP,
    transmitBP,
    distBPs,
    generateBP,
    isFresh
) where

import GHC.Exts (fromList, toList)
import qualified Data.Foldable as F
import qualified BellKAT.Utils.Multiset              as Mset
import Control.Subcategory.Pointed
import Data.Default

import BellKAT.Utils.Distribution as D hiding (Probability)
import BellKAT.Definitions.Core
import BellKAT.Implementations.Output
import BellKAT.Utils.Multiset (labelledMempty)
import BellKAT.Utils.Convex
import qualified Data.Aeson as A
import           Data.Aeson ((.=), (.:))
-- Removed: import Control.Monad.Writer.Strict

type SpaceUnit = Int     -- discrete and fixed (L) space unit
type TimeUnit = Int      -- discrete and fixed (L/c) time unit
type Werner = Double     -- representing fidelity, in the range [0,1]

-- | TODO: refactor as something to be set in the DSL
-- | If True, swaps are considered instantaneous (no time delay)
instantaneousOps :: Bool
#ifdef INSTANTANEOUS_OPS_FALSE
instantaneousOps = False
#else
instantaneousOps = True
#endif

-- | Clock wrapper 
newtype MaxClock = MaxClock { getMaxClock :: TimeUnit }
    deriving stock (Eq, Ord)

instance Semigroup MaxClock where
    MaxClock a <> MaxClock b = MaxClock (max a b)

-- | Monoid identity is 0
instance Monoid MaxClock where
    mempty = MaxClock 0
    mappend = (<>)

instance Show MaxClock where
    show (MaxClock t) = "(" ++ show t ++ ")"

instance A.ToJSON MaxClock where
    toJSON (MaxClock t) = A.toJSON t

instance A.FromJSON MaxClock where
    parseJSON v = MaxClock <$> A.parseJSON v

-- | A quantum tag for Bell pairs
data QuantumTag = QuantumTag
    { qtTimestamp  :: TimeUnit -- timestamp of production of the BP
    , qtFidelity :: Werner     -- quality at the time of production of the BP
    }
    deriving stock (Eq, Ord)

instance Show QuantumTag where
    show (QuantumTag t w) = "{w=" ++ show w ++ ", t=" ++ show t ++ "}"

instance Default QuantumTag where
    def = QuantumTag 0 0.958
                     {- ^ example to see fidelity evolving with swap -}

instance A.ToJSON QuantumTag where
    toJSON (QuantumTag t w) = A.object ["time" .= t, "werner" .= w]

instance A.FromJSON QuantumTag where
    parseJSON = A.withObject "QuantumTag" $ \o ->
        QuantumTag <$> o .: "time" <*> o .: "werner"

data QuantumOutput = QuantumOutput { qoOutputBP :: TaggedBellPair (), qoOperation :: Op }
    deriving stock (Eq, Ord)

instance Show QuantumOutput where
    show (QuantumOutput bp op) = "(" ++ show bp ++ ", " ++ show op ++ ")"

instance RuntimeTag QuantumTag () where
  staticTag _ = ()

instance Output QuantumOutput where
    type STag QuantumOutput = ()
    type RTag QuantumOutput = QuantumTag
    type CTag QuantumOutput = MaxClock
    type OutputM QuantumOutput = CD'
    computeOutput QuantumOutput{qoOperation = FSkip} (Mset.LMS (_, clock)) =
        cpure (labelledMempty clock)

    computeOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FCreate p w} inClockedBps =
        [createBP p inClockedBps (bellPair outBp @ QuantumTag 0 w)]

    computeOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FGenerate p w d} inClockedBps =
        [generateBP p d inClockedBps $ bellPair outBp @ QuantumTag 1 w]

    computeOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FTransmit p tCohs d} inClockedBps =
        [transmitBP p tCohs d inClockedBps $ bellPair outBp]

    computeOutput QuantumOutput{qoOperation = FDestroy} (Mset.LMS (_, clock)) =
        [cpure (labelledMempty clock)]

    computeOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FSwap p tCohs ds} inClockedBps =
        [swapBPs p tCohs ds inClockedBps outBp]

    computeOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FDistill tCohs d} inClockedBps =
        [distBPs tCohs d inClockedBps outBp]

instance OpOutput QuantumOutput Op where
    fromCBPOutput _ bp op = QuantumOutput { qoOutputBP = bp, qoOperation = op }

-- | Swap two Bell pairs and returns a distribution D' 
-- | with probability p (the success probability) the output is a new tagged Bell pair connecting the two end nodes, 
-- | and with probability 1-p the swap fails
-- | On failure, no entangled pair remains (both inputs are destroyed in the process). 
-- | Note: it fails if not exactly two bell pairs are given in input
swapBPs :: Rational
            -> (TimeUnit, TimeUnit, TimeUnit)
            -> (SpaceUnit, SpaceUnit)
            -> LabelledBellPairs MaxClock QuantumTag
            -> TaggedBellPair tag
            -> D' (LabelledBellPairs MaxClock QuantumTag)
swapBPs p (tCohL, tCohL1, tCohL2) (d1, d2) (Mset.LMS (inBps, clock)) (TaggedBellPair outBp _) = 
          {- ^ swap node -}
    case toList inBps of
        [TaggedBellPair _ (QuantumTag t1 w1), TaggedBellPair _ (QuantumTag t2 w2)] ->
            let
                completionTime = if instantaneousOps then 0 else max d1 d2
                productionTS = getMaxClock clock + completionTime
                newTag = QuantumTag
                    { qtTimestamp = productionTS
                    , qtFidelity  = w1 * w2 * decay (tCohL,  tCohL1) (getMaxClock clock - t1)
                                            * decay (tCohL,  tCohL2) (getMaxClock clock - t2)
                                            * decay (tCohL1, tCohL2) completionTime
                    }
                successOutput = Mset.singletonT (TaggedBellPair outBp newTag) (MaxClock productionTS)
                failureOutput = labelledMempty (MaxClock productionTS)
            in case p of
                0 -> cpure failureOutput
                1 -> cpure successOutput
                _ -> fromList [ (successOutput, p), (failureOutput, 1 - p) ]
        _ -> error "swapBPs: expected exactly two input tagged Bell pairs"


-- | Perform entanglement distillation on two tagged Bell pairs.
-- | returns a distribution capturing the probabilistic nature of entanglement distillation. 
-- | succeeds with probability `pDist = (1 + wA * wB) / 2`
-- | to yield one new Bell pair with improved fidelity `wDist = (wA + wB + 4 * wA * wB) / (6 * pDist)`
-- | and fails with the remaining probability (yielding no output pair, as the two input pairs are consumed)
-- | Note: it fails if not exactly two bell pairs are given in input
distBPs :: (TimeUnit, TimeUnit)
        -> SpaceUnit
        -> LabelledBellPairs MaxClock QuantumTag
        -> TaggedBellPair ()
        -> D' (LabelledBellPairs MaxClock QuantumTag)
distBPs (tCoh1, tCoh2) d (Mset.LMS (inBps, clock)) (TaggedBellPair outBp _) =
    case toList inBps of
        [TaggedBellPair _ (QuantumTag t1 w1), TaggedBellPair _ (QuantumTag t2 w2)] ->
            let
                pDistD :: Double
                pDistD = (1 + w1 * w2) / 2
                wDistD :: Double
                wDistD = (w1 + w2 + 4 * w1 * w2) / (6 * pDistD)
                completionTime = if instantaneousOps then 0 else d
                productionTS = getMaxClock clock + completionTime
                newTag = QuantumTag
                    { qtTimestamp = productionTS
                    , qtFidelity  = wDistD * decay (tCoh1, tCoh2) (getMaxClock clock - t1)
                                           * decay (tCoh1, tCoh2) (getMaxClock clock - t2)
                    }
                successOutput = Mset.singletonT (TaggedBellPair outBp newTag) (MaxClock productionTS)
                failureOutput = labelledMempty (MaxClock productionTS)
            in case pDistD of
                0 -> cpure failureOutput
                1 -> cpure successOutput
                _ -> fromList [ (successOutput, toRational pDistD), (failureOutput, 1 - toRational pDistD) ]
        _ -> error "distBPs: expected exactly two input tagged Bell pairs"


-- | Auxiliary function
-- | With probability p it yields one new TaggedBellPair QuantumTag (the output Bell pair with a new tag), 
-- | and with probability 1-p it yields no output (representing failure). 
produceBP :: Rational
        -> TaggedBellPair QuantumTag
        -> MaxClock
        -> D' (LabelledBellPairs MaxClock QuantumTag)
produceBP p outBp outClock
        | p == 0 = cpure (labelledMempty outClock)
        | p == 1 = cpure (Mset.singletonT outBp outClock)
        | otherwise = D.choose p (Mset.singletonT outBp outClock) (labelledMempty outClock)

-- | Create: Output = Create p BellPair(loc) 
-- | creation of a new Bell pair at node loc (both ends same), 
-- | expecting no input Bell pairs.
-- | New tag's timestamp is set to a base value (we use 0) since this is a freshly created pair. 
-- | Fidelity is initialized to a default baseline or configured value.
createBP :: Rational
            -> LabelledBellPairs MaxClock QuantumTag
            -> TaggedBellPair QuantumTag
            -> D' (LabelledBellPairs MaxClock QuantumTag)
createBP p (Mset.LMS (inBps, clock)) (TaggedBellPair outBp (QuantumTag t w0)) =
    if F.null inBps then
        let productionTS = getMaxClock clock + t
            newTag = TaggedBellPair outBp (QuantumTag productionTS w0)
        in produceBP p newTag (MaxClock productionTS)
    else
        error "createBP: expected empty input"

-- | Transmit: For Output = Transmit p BellPair(locA~locB)
-- | transmission of one end of a local pair from src to a remote node dest, 
-- | one input tagged pair (the local one) is expected.
-- | The output tag's time is set to (input.qtTimestamp + 1) to model the delay of transmission. 
-- | The fidelity of the output pair decays 1.
transmitBP :: Rational
           -> (TimeUnit, TimeUnit)
           -> SpaceUnit
           -> LabelledBellPairs MaxClock QuantumTag
           -> BellPair
           -> D' (LabelledBellPairs MaxClock QuantumTag)
transmitBP p (tCoh1, tCoh2) d (Mset.LMS (inBps, clock)) outBp =
    case toList inBps of
        [TaggedBellPair _ (QuantumTag t w)] ->
                                     {- ^ pass on the Werner parameter -}
            let productionTS = getMaxClock clock + d
                newTag = TaggedBellPair outBp (QuantumTag productionTS (w * decay (tCoh1, tCoh2) (getMaxClock clock - t)))
            in produceBP p newTag (MaxClock productionTS)
        _ -> error "transmitBP: expected exactly one input tagged Bell pair"

-- | UnstableCreate: Output = UnstableCreate p BellPair(locA~locB) 
-- | generation (creation and transmission combined) of a new Bell pair. 
-- | one input tagged pair (the local one) is expected.
-- | The new tag's timestamp is set to a base value (we use 1) since this is a freshly generated pair. 
-- | Its fidelity is initialized to a default baseline or configured value.
generateBP :: Rational
           -> SpaceUnit
           -> LabelledBellPairs MaxClock QuantumTag
           -> TaggedBellPair QuantumTag
           -> D' (LabelledBellPairs MaxClock QuantumTag)
generateBP p d (Mset.LMS (inBps, clock)) (TaggedBellPair outBp (QuantumTag _ w0)) =
    if F.null inBps then
        let productionTS = getMaxClock clock + d
            newTag  = TaggedBellPair outBp (QuantumTag productionTS w0)
        in produceBP p newTag (MaxClock productionTS)
    else
        error "generateBP: expected empty input"

-- | Memory decay of a pair in function of time passed and coherence time
decay:: (TimeUnit, TimeUnit) -> TimeUnit -> Double
decay (tCohLinkA, tCohLinkB) deltaT = 
    exp (- fromIntegral deltaT / fromIntegral tCohLinkA - fromIntegral deltaT / fromIntegral tCohLinkB)

-- | Check if a pair is 'fresh enough' (where Nothing means no check)
isFresh :: TaggedBellPair QuantumTag -> MaxClock -> Maybe TimeUnit -> Bool
isFresh _ _ Nothing = True
isFresh (TaggedBellPair _ (QuantumTag t _)) clock (Just tCut) =
    let age = getMaxClock clock - t
     in age <= tCut
