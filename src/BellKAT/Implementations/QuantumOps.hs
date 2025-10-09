{-# LANGUAGE OverloadedLists #-}

{- |
   Module : BellKAT.Definitions.QuantumOps
   Description : Syntactic definitions related to quantum operations
-}
module BellKAT.Implementations.QuantumOps (
    -- * Quantum tags
    QuantumTag(..),
    MaxClock(..),
    TimeUnit,
    Werner,
    -- * Primitive quantum operations (exported for testing)
    swapBPs,
    createBP,
    transmitBP,
    distBPs,
    generateBP,
    decayWerner,
    tCoherence
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
import Data.Semigroup ()
import qualified Data.Aeson as A

type TimeUnit = Int      -- discrete and fixed (L/c) time unit
type Werner = Double     -- representing fidelity, in the range [0,1]

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

tCoherence :: TimeUnit
tCoherence = 100

-- | A quantum tag for Bell pairs
data QuantumTag = QuantumTag
    { qtTimestamp  :: TimeUnit -- timestamp of production of the BP
    , qtFidelity :: Werner     -- quality at the time of production of the BP
    }
    deriving stock (Eq, Ord)

instance Show QuantumTag where
    show (QuantumTag t w) = "{w=" ++ show w ++ ", t=" ++ show t ++ "}"

instance Default QuantumTag where
    def = QuantumTag 0 0.8
                     {- ^ example to see fidelity evolving with swap -}

instance RuntimeTag QuantumTag () where
  staticTag _ = ()

instance Output (TaggedBellPair (), Op QuantumTag) () where
    type RTag (TaggedBellPair (), Op QuantumTag) = QuantumTag
    type CTag (TaggedBellPair (), Op QuantumTag) = MaxClock
    computeOutput (_, FSkip) (Mset.LMS (_, clock)) =
        cpure (labelledMempty clock)

    computeOutput (outBp, FCreate p t) inClockedBps = 
        [createBP p inClockedBps (bellPair outBp @ t)]

    computeOutput (outBp, FGenerate p t) inClockedBps = 
        [generateBP p inClockedBps $ bellPair outBp @ t]

    computeOutput (outBp, FTransmit p t) inClockedBps =
        [transmitBP p inClockedBps $ bellPair outBp @ t]

    computeOutput (_, FDestroy) (Mset.LMS (_, clock)) =
        [cpure (labelledMempty clock)]

    computeOutput (outBp, FSwap p) inClockedBps =
        [swapBPs p inClockedBps outBp]

    computeOutput (outBp, FDistill) inClockedBps =
        [distBPs inClockedBps outBp]

instance OpOutput (TaggedBellPair (), Op QuantumTag) (Op QuantumTag) () where
    fromCBPOutput _ bp op = (bp, op)

-- | Swap two Bell pairs and returns a distribution D' 
-- | with probability p (the success probability) the output is a new tagged Bell pair connecting the two end nodes, 
-- | and with probability 1-p the swap fails
-- | On failure, no entangled pair remains (both inputs are destroyed in the process). 
-- | Note: it fails if not exactly two bell pairs are given in input
swapBPs :: Rational
            -> LabelledBellPairs MaxClock QuantumTag
            -> TaggedBellPair tag
            -> D' (LabelledBellPairs MaxClock QuantumTag)
swapBPs p (Mset.LMS (inBps, clock)) (TaggedBellPair outBp _) = 
    case toList inBps of
        [TaggedBellPair _ (QuantumTag t1 w1), TaggedBellPair _ (QuantumTag t2 w2)] ->
            let
                productionTS = getMaxClock clock + max t1 t2 + 1
                newTag = QuantumTag
                    { qtTimestamp = productionTS
                    , qtFidelity  = decayWerner (abs (t1 - t2)) (w1 * w2)
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
distBPs :: LabelledBellPairs MaxClock QuantumTag
        -> TaggedBellPair ()
        -> D' (LabelledBellPairs MaxClock QuantumTag)
distBPs (Mset.LMS (inBps, clock)) (TaggedBellPair outBp _) =
    case toList inBps of
        [TaggedBellPair _ (QuantumTag t1 w1), TaggedBellPair _ (QuantumTag t2 w2)] ->
            let
                pDistD :: Double
                pDistD = (1 + w1 * w2) / 2
                wDistD :: Double
                wDistD = (w1 + w2 + 4 * w1 * w2) / (6 * pDistD)
                productionTS = getMaxClock clock + max t1 t2 + 1
                newTag = QuantumTag
                    { qtTimestamp = productionTS
                    , qtFidelity  = decayWerner (abs (t1 - t2)) wDistD
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
produceBP p outBp clock
        | p == 0 = cpure (labelledMempty clock)
        | p == 1 = cpure (Mset.singletonT outBp clock)
        | otherwise = D.choose p (Mset.singletonT outBp clock) (labelledMempty clock)

-- | Create: Output = Create p BellPair(loc) 
-- | creation of a new Bell pair at node loc (both ends same), 
-- | expecting no input Bell pairs.
-- | New tag's timestamp is set to a base value (we use 0) since this is a freshly created pair. 
-- | Fidelity is initialized to a default baseline or configured value.
createBP :: Rational
            -> LabelledBellPairs MaxClock QuantumTag
            -> TaggedBellPair QuantumTag
            -> D' (LabelledBellPairs MaxClock QuantumTag)
createBP p (Mset.LMS (inBps, clock)) (TaggedBellPair outBp _) = 
    if F.null inBps then 
        let newTag = TaggedBellPair outBp def -- TODO: update by getting fidelity from config
        in produceBP p newTag clock
    else
        error "createBP: expected empty input"

-- | Transmit: For Output = Transmit p BellPair(locA~locB)
-- | transmission of one end of a local pair from src to a remote node dest, 
-- | one input tagged pair (the local one) is expected.
-- | The output tag's time is set to (input.qtTimestamp + 1) to model the delay of transmission. 
-- | The fidelity of the output pair decays 1.
transmitBP :: Rational
           -> LabelledBellPairs MaxClock QuantumTag
           -> TaggedBellPair QuantumTag
           -> D' (LabelledBellPairs MaxClock QuantumTag)
transmitBP p (Mset.LMS (inBps, clock)) (TaggedBellPair outBp _) =
    case toList inBps of
        [TaggedBellPair _ (QuantumTag t w)] ->
            let newTag = TaggedBellPair outBp (QuantumTag (t + 1) (decayWerner 1 w))
            in produceBP p newTag clock
        _ -> error "transmitBP: expected exactly one input tagged Bell pair"

-- | UnstableCreate: Output = UnstableCreate p BellPair(locA~locB) 
-- | generation (creation and transmission combined) of a new Bell pair. 
-- | one input tagged pair (the local one) is expected.
-- | The new tag's timestamp is set to a base value (we use 1) since this is a freshly generated pair. 
-- | Its fidelity is initialized to a default baseline or configured value.
generateBP :: Rational
           -> LabelledBellPairs MaxClock QuantumTag
           -> TaggedBellPair QuantumTag
           -> D' (LabelledBellPairs MaxClock QuantumTag)
generateBP p (Mset.LMS (inBps, clock)) (TaggedBellPair outBp _) =
    if F.null inBps then
        let baseTag = def :: QuantumTag
            newTag  = TaggedBellPair outBp baseTag { qtTimestamp = 1 } -- TODO: update by getting fidelity from config
        in produceBP p newTag clock
    else
        error "generateBP: expected empty input"

-- | Memory decoherence, in function of time
decayWerner:: TimeUnit -> Werner -> Werner
decayWerner deltaT w = w * exp (-fromIntegral deltaT / fromIntegral tCoherence)
