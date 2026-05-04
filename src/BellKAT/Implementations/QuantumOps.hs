{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
   Module : BellKAT.Implementations.QuantumOps
   Description : Syntactic definitions related to quantum operations
-}
module BellKAT.Implementations.QuantumOps (
    -- * Quantum tags
    QuantumTag(..),
    qtDistillations,
    qtTimestamp,
    qtFidelity,
    QuantumOutput(..),
    MaxClock(..),
    SpaceUnit,
    TimeUnit,
    Werner,
    -- * Primitive quantum operations (exported for testing)
    swapBPs,
    simSwapBPs,
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
import BellKAT.Implementations.ProbabilisticQuantumOps (DistillationCount)
import BellKAT.Utils.Multiset (labelledMempty)
import BellKAT.Utils.Convex
import qualified Data.Aeson as A
import           Data.Aeson ((.=), (.:), (.:?))
-- Removed: import Control.Monad.Writer.Strict

type SpaceUnit = Int     -- discrete and fixed (L) space unit
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

-- | A quantum tag for Bell pairs
-- The two-field constructor is kept for existing examples and represents the
-- default static distillation count
data QuantumTag
    = QuantumTag TimeUnit Werner
    | TaggedQuantumTag DistillationCount TimeUnit Werner
    deriving stock (Eq, Ord)

qtDistillations :: QuantumTag -> DistillationCount
qtDistillations (QuantumTag _ _) = def
qtDistillations (TaggedQuantumTag count _ _) = count

qtTimestamp :: QuantumTag -> TimeUnit
qtTimestamp (QuantumTag t _) = t
qtTimestamp (TaggedQuantumTag _ t _) = t

qtFidelity :: QuantumTag -> Werner
qtFidelity (QuantumTag _ w) = w
qtFidelity (TaggedQuantumTag _ _ w) = w

mkQuantumTag :: DistillationCount -> TimeUnit -> Werner -> QuantumTag
mkQuantumTag count t w
  | count == def = QuantumTag t w
  | otherwise = TaggedQuantumTag count t w

instance Show QuantumTag where
    show tag =
        "{"
        ++ countPart
        ++ "w=" ++ show (qtFidelity tag)
        ++ ", t=" ++ show (qtTimestamp tag)
        ++ "}"
      where
        count = qtDistillations tag
        countPart
          | count == def = ""
          | otherwise = "d=" ++ show count ++ ", "

instance Default QuantumTag where
    def = QuantumTag 0 0.958
                     {- ^ example to see fidelity evolving with swap -}

instance A.ToJSON QuantumTag where
    toJSON tag =
        A.object $
            [ "time" .= qtTimestamp tag
            , "werner" .= qtFidelity tag
            ]
            <> [ "distillations" .= count | count /= def ]
      where
        count = qtDistillations tag

instance A.FromJSON QuantumTag where
    parseJSON = A.withObject "QuantumTag" $ \o ->
        mkQuantumTag
            <$> (o .:? "distillations" >>= pure . maybe def id)
            <*> o .: "time"
            <*> o .: "werner"

data QuantumOutput = QuantumOutput { qoOutputBP :: TaggedBellPair DistillationCount, qoOperation :: Op }
    deriving stock (Eq, Ord)

instance Show QuantumOutput where
    show (QuantumOutput bp op) = "(" ++ show bp ++ ", " ++ show op ++ ")"

instance RuntimeTag QuantumTag () where
  staticTag _ = ()

instance RuntimeTag QuantumTag DistillationCount where
  staticTag = qtDistillations

instance Output QuantumOutput where
    type STag QuantumOutput = DistillationCount
    type RTag QuantumOutput = QuantumTag
    type CTag QuantumOutput = MaxClock
    type OutputM QuantumOutput = CD'
    computeOutput QuantumOutput{qoOperation = FSkip} (Mset.LMS (_, clock)) =
        cpure (labelledMempty clock)

    computeOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FCreate p w} inClockedBps =
        [createBP p inClockedBps (bellPair outBp @ mkQuantumTag (bellPairTag outBp) 0 w)]

    computeOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FGenerate p w d} inClockedBps =
        [generateBP p d inClockedBps $ bellPair outBp @ mkQuantumTag (bellPairTag outBp) 1 w]

    computeOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FTransmit p tCohs d} inClockedBps =
        [transmitBP p tCohs d inClockedBps outBp]

    computeOutput QuantumOutput{qoOperation = FDestroy} (Mset.LMS (_, clock)) =
        [cpure (labelledMempty clock)]

    computeOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FSwap p tCohs ds} inClockedBps =
        [swapBPs p tCohs ds inClockedBps outBp]

    computeOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FSimSwap p coherenceSpecs distanceSpecs} inClockedBps =
        [simSwapBPs p coherenceSpecs distanceSpecs inClockedBps outBp]

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
            -> TaggedBellPair DistillationCount
            -> D' (LabelledBellPairs MaxClock QuantumTag)
swapBPs p (tCohL, tCohL1, tCohL2) (d1, d2) (Mset.LMS (inBps, clock)) (TaggedBellPair outBp _) = 
          {- ^ swap node -}
    case toList inBps of
        [TaggedBellPair _ qTag1, TaggedBellPair _ qTag2] ->
            let
                t1 = qtTimestamp qTag1
                t2 = qtTimestamp qTag2
                w1 = qtFidelity qTag1
                w2 = qtFidelity qTag2
                outCount = max (qtDistillations qTag1) (qtDistillations qTag2)
                completionTime = max d1 d2
                productionTS = getMaxClock clock + completionTime
                newTag = mkQuantumTag outCount productionTS $
                    w1 * w2 * decay (tCohL,  tCohL1) (getMaxClock clock - t1)
                            * decay (tCohL,  tCohL2) (getMaxClock clock - t2)
                            * decay (tCohL1, tCohL2) completionTime
                successOutput = Mset.singletonT (TaggedBellPair outBp newTag) (MaxClock productionTS)
                failureOutput = labelledMempty (MaxClock productionTS)
            in case p of
                0 -> cpure failureOutput
                1 -> cpure successOutput
                _ -> fromList [ (successOutput, p), (failureOutput, 1 - p) ]
        _ -> error "swapBPs: expected exactly two input tagged Bell pairs"

-- | Simultaneously swap a whole chain of Bell pairs into one end-to-end pair
-- | Success consumes all inputs and produces the output pair 
-- | with probability equal to the product of the individual repeater swap probabilities.
simSwapBPs :: Rational
            -> ([(BellPair, (TimeUnit, TimeUnit))], (TimeUnit, TimeUnit))
            -> [(BellPair, SpaceUnit)]
            -> LabelledBellPairs MaxClock QuantumTag
            -> TaggedBellPair DistillationCount
            -> D' (LabelledBellPairs MaxClock QuantumTag)
simSwapBPs p (edgeCohSpecs, endpointCohs) distanceSpecs (Mset.LMS (inBps, clock)) (TaggedBellPair outBp _)
    | null edgeCohSpecs =
        error "simSwapBPs: expected at least one input edge"
    | length inputBps /= length edgeCohSpecs =
        error $ "simSwapBPs: expected exactly " <> show (length edgeCohSpecs)
             <> " input tagged Bell pairs, got " <> show (length inputBps)
    | fmap fst edgeCohSpecs /= fmap fst distanceSpecs =
        error "simSwapBPs: coherence and distance specs must describe the same chain"
    | otherwise =
        case matchInputEdges edgeCohSpecs inputBps of
            Nothing ->
                error "simSwapBPs: input Bell pairs do not match the simultaneous-swap chain"
            Just matched ->
                let
                    clockNow = getMaxClock clock
                    completionTime = sum (fmap snd distanceSpecs)
                    productionTS = clockNow + completionTime
                    inputWernerProduct =
                        product [ qtFidelity qTag | (_, qTag, _) <- matched ]
                    inputDecayProduct =
                        product
                            [ decay edgeCohs (clockNow - qtTimestamp qTag)
                            | (_, qTag, edgeCohs) <- matched
                            ]
                    outCount =
                        maximum [ qtDistillations qTag | (_, qTag, _) <- matched ]
                    newTag = mkQuantumTag outCount productionTS $
                        inputWernerProduct
                            * inputDecayProduct
                            * decay endpointCohs completionTime
                    successOutput = Mset.singletonT (TaggedBellPair outBp newTag) (MaxClock productionTS)
                    failureOutput = labelledMempty (MaxClock productionTS)
                in case p of
                    0 -> cpure failureOutput
                    1 -> cpure successOutput
                    _ -> fromList [ (successOutput, p), (failureOutput, 1 - p) ]
  where
    inputBps = toList inBps

    matchInputEdges [] [] = Just []
    matchInputEdges [] _ = Nothing
    matchInputEdges _ [] = Nothing
    matchInputEdges ((edgeBp, edgeCohs) : edges) bps = do
        (TaggedBellPair _ qTag, remaining) <- pickEdge edgeBp bps
        matched <- matchInputEdges edges remaining
        pure ((edgeBp, qTag, edgeCohs) : matched)

    pickEdge _ [] = Nothing
    pickEdge edgeBp (bp@(TaggedBellPair actualBp _) : bps)
        | actualBp == edgeBp = Just (bp, bps)
        | otherwise = do
            (found, remaining) <- pickEdge edgeBp bps
            pure (found, bp : remaining)

-- | Perform entanglement distillation on two tagged Bell pairs.
-- | returns a distribution capturing the probabilistic nature of entanglement distillation. 
-- | succeeds with probability `pDist = (1 + wA * wB) / 2`
-- | to yield one new Bell pair with improved fidelity `wDist = (wA + wB + 4 * wA * wB) / (6 * pDist)`
-- | and fails with the remaining probability (yielding no output pair, as the two input pairs are consumed)
-- | Note: it fails if not exactly two bell pairs are given in input
distBPs :: (TimeUnit, TimeUnit)
        -> SpaceUnit
        -> LabelledBellPairs MaxClock QuantumTag
        -> TaggedBellPair DistillationCount
        -> D' (LabelledBellPairs MaxClock QuantumTag)
distBPs (tCoh1, tCoh2) d (Mset.LMS (inBps, clock)) (TaggedBellPair outBp _) =
    case toList inBps of
        [TaggedBellPair _ qTag1, TaggedBellPair _ qTag2] ->
            let
                t1 = qtTimestamp qTag1
                t2 = qtTimestamp qTag2
                w1 = qtFidelity qTag1
                w2 = qtFidelity qTag2
                outCount = max (qtDistillations qTag1) (qtDistillations qTag2) + 1
                pDistD :: Double
                pDistD = (1 + w1 * w2) / 2
                wDistD :: Double
                wDistD = (w1 + w2 + 4 * w1 * w2) / (6 * pDistD)
                completionTime = d
                productionTS = getMaxClock clock + completionTime
                newTag = mkQuantumTag outCount productionTS $
                    wDistD * decay (tCoh1, tCoh2) (getMaxClock clock - t1)
                           * decay (tCoh1, tCoh2) (getMaxClock clock - t2)
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
createBP p (Mset.LMS (inBps, clock)) (TaggedBellPair outBp qTag) =
    if F.null inBps then
        let productionTS = getMaxClock clock + qtTimestamp qTag
            newTag = TaggedBellPair outBp (mkQuantumTag (qtDistillations qTag) productionTS (qtFidelity qTag))
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
           -> TaggedBellPair DistillationCount
           -> D' (LabelledBellPairs MaxClock QuantumTag)
transmitBP p (tCoh1, tCoh2) d (Mset.LMS (inBps, clock)) outBp =
    case toList inBps of
        [TaggedBellPair _ qTag] ->
                                     {- ^ pass on the Werner parameter -}
            let t = qtTimestamp qTag
                w = qtFidelity qTag
                productionTS = getMaxClock clock + d
                newTag = TaggedBellPair (bellPair outBp) $
                    mkQuantumTag (qtDistillations qTag) productionTS (w * decay (tCoh1, tCoh2) (getMaxClock clock - t))
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
generateBP p d (Mset.LMS (inBps, clock)) (TaggedBellPair outBp qTag) =
    if F.null inBps then
        let productionTS = getMaxClock clock + d
            newTag  = TaggedBellPair outBp (mkQuantumTag (qtDistillations qTag) productionTS (qtFidelity qTag))
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
isFresh (TaggedBellPair _ qTag) clock (Just tCut) =
    let t = qtTimestamp qTag
        age = getMaxClock clock - t
     in age <= tCut
