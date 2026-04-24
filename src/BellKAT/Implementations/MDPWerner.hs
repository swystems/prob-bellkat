{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Implementations.MDPWerner
    ( WernerBellPairs(..)
    , toWernerBellPairs
    , holdsWernerTest
    , holdsWernerGuardTest
    , StepCost(..)
    , MDP(..)
    , MDP'
    , minimizeStateSystem
    , execute
    , execute'
    , executeWith'
    ) where

import           Control.Subcategory.Applicative
import           Control.Subcategory.Functor
import           Control.Subcategory.Pointed
import           Data.Bifunctor              (second)
import           Data.Default
import qualified Data.Map.Strict             as Map
import qualified GHC.Exts
import           GHC.Exts                    (fromList, toList)

import qualified BellKAT.Utils.Multiset      as Mset
import           BellKAT.ActionEmbeddings    (ProbabilisticActionConfiguration(..))
import           BellKAT.Definitions.Atomic
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Tests
import           BellKAT.Implementations.Configuration
import           BellKAT.Implementations.Output
import           BellKAT.Implementations.ProbAtomicOneStepQuantum (ProbAtomicOneStepPolicy)
import           BellKAT.Implementations.ProbabilisticQuantumOps
    ( BinaryOutput(..)
    , DistillationCount
    , StateKind(..)
    , WernerTag(..)
    )
import           BellKAT.Utils.Choice
import           BellKAT.Utils.Distribution  (D, RationalOrDouble)
import           BellKAT.Utils.MDP
    ( MDP(..)
    , MDP'
    , StepCost(..)
    , combinedRoundCost
    , fromDistribution
    , mapMDPProbabilities
    , minimizeStateSystem
    , parallelCompose
    , requireCardinality
    , setAllCosts
    )
import qualified BellKAT.Utils.Distribution  as D

-- | Wrapper for pairs with a static distillation count and runtime purity tag
newtype WernerBellPairs = WernerBellPairs { unWernerBellPairs :: TaggedBellPairs WernerTag }
    deriving stock (Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance GHC.Exts.IsList WernerBellPairs where
    type Item WernerBellPairs = TaggedBellPair WernerTag
    fromList = WernerBellPairs . GHC.Exts.fromList
    toList = GHC.Exts.toList . Mset.bellPairs . unWernerBellPairs

instance Show WernerBellPairs where
    show (WernerBellPairs bps) =
        "⦃" <> renderPairs (toList (Mset.bellPairs bps)) <> "⦄"
      where
        renderPairs [] = ""
        renderPairs [x] = renderPair x
        renderPairs (x:xs) = renderPair x <> "," <> renderPairs xs

        renderPair :: TaggedBellPair WernerTag -> String
        renderPair (TaggedBellPair bp WernerTag{wtDistillations, wtStateKind}) =
            let (l1, l2) = locations bp
                sep = case wtStateKind of
                    Pure -> "-"
                    Mixed -> "="
                countSuffix
                  | wtDistillations == def = ""
                  | otherwise = show wtDistillations
             in name l1 <> sep <> name l2 <> countSuffix

-- | When applying capacity constraints, 
-- | we keep track of which pairs are produced by the current action 
-- | and which are retained from the input
data WernerPieces = WernerPieces
    { wpProduced :: WernerBellPairs
    , wpRetained :: WernerBellPairs
    } deriving stock (Eq, Ord, Show)

instance Semigroup WernerPieces where
    WernerPieces p1 r1 <> WernerPieces p2 r2 =
        -- | we keep the produced pieces first (better quality)
        WernerPieces (p1 <> p2) (r1 <> r2)

instance Monoid WernerPieces where
    mempty = WernerPieces mempty mempty

producedPieces :: WernerBellPairs -> WernerPieces
producedPieces bps = WernerPieces bps mempty

retainedPieces :: WernerBellPairs -> WernerPieces
retainedPieces bps = WernerPieces mempty bps

finalizeWernerPieces :: Maybe (NetworkCapacity DistillationCount) -> WernerPieces -> WernerBellPairs
finalizeWernerPieces Nothing (WernerPieces produced retained) =
    produced <> retained
finalizeWernerPieces (Just cap) (WernerPieces produced retained) =
    WernerBellPairs . (Mset.@ ()) . Mset.fromList $ producedKept <> retainedKept
  where
    (producedKept, remainingCounts) =
        takeWernerCapacity (capacityCounts cap) (toList produced)
    (retainedKept, _) =
        takeWernerCapacity remainingCounts (toList retained)

capacityCounts :: NetworkCapacity DistillationCount -> Map.Map BellPair Int
capacityCounts (NC cap) =
    Map.fromListWith (+) [ (bellPair tbp, 1) | tbp <- toList cap ]

takeWernerCapacity
    :: Map.Map BellPair Int
    -> [TaggedBellPair WernerTag]
    -> ([TaggedBellPair WernerTag], Map.Map BellPair Int)
takeWernerCapacity = go []
  where
    go kept counts [] = (reverse kept, counts)
    go kept counts (tbp:tbps) =
        case Map.lookup bp counts of
            Nothing ->
                go (tbp:kept) counts tbps
            Just n
              | n > 0 ->
                  go (tbp:kept) (Map.insert bp (n - 1) counts) tbps
              | otherwise ->
                  go kept counts tbps
      where
        bp = bellPair tbp

toWernerBellPairs :: LabelledBellPairs cTag WernerTag -> WernerBellPairs
toWernerBellPairs = WernerBellPairs . Mset.map' id

-- | Evaluates test on both static distillation counts and runtime purity tags
holdsWernerTest :: KindedTest DistillationCount -> WernerBellPairs -> Bool
holdsWernerTest t = getBPsPredicate (toSelectorPredicate t) . expandSelectorState . unWernerBellPairs

-- | Evaluates test ignoring the purity tags
holdsWernerGuardTest :: Test test => test DistillationCount -> WernerBellPairs -> Bool
holdsWernerGuardTest t = getBPsPredicate (toBPsPredicate t) . staticBellPairs . unWernerBellPairs

-- | Expands into selectors for the static count and observed purity of each pair
expandSelectorState :: TaggedBellPairs WernerTag -> TaggedBellPairs (PairSelector DistillationCount)
expandSelectorState (Mset.LMS (bps, ())) =
    Mset.fromList (concatMap expandPair (GHC.Exts.toList bps)) Mset.@ ()
  where
    expandPair (TaggedBellPair bp WernerTag{wtDistillations, wtStateKind}) =
        [ TaggedBellPair bp (StaticPair wtDistillations)
        , TaggedBellPair bp $
            case wtStateKind of
                Pure -> PurePair wtDistillations
                Mixed -> MixedPair wtDistillations
        ]

execute
    :: ProbabilisticActionConfiguration
    -> ProbAtomicOneStepPolicy (ListOutput BinaryOutput) DistillationCount
    -> WernerBellPairs
    -> MDP' WernerBellPairs
execute = execute'

execute'
    :: RationalOrDouble p
    => ProbabilisticActionConfiguration
    -> ProbAtomicOneStepPolicy (ListOutput BinaryOutput) DistillationCount
    -> WernerBellPairs
    -> MDP p WernerBellPairs
execute' pac policy =
    mapMDPProbabilities realToFrac . executeDouble pac Nothing policy

executeWith'
    :: RationalOrDouble p
    => ProbabilisticActionConfiguration
    -> ExecutionParams DistillationCount rTag cTag
    -> ProbAtomicOneStepPolicy (ListOutput BinaryOutput) DistillationCount
    -> WernerBellPairs
    -> MDP p WernerBellPairs
executeWith' pac ep policy =
    mapMDPProbabilities realToFrac . executeDouble pac (epNetworkCapacity ep) policy

executeDouble
    :: ProbabilisticActionConfiguration
    -> Maybe (NetworkCapacity DistillationCount)
    -> ProbAtomicOneStepPolicy (ListOutput BinaryOutput) DistillationCount
    -> WernerBellPairs
    -> MDP Double WernerBellPairs
executeDouble pac mbCap policy bps =
    foldMap (\paa -> executePAA pac mbCap paa bps) (toList policy)

executePAA
    :: ProbabilisticActionConfiguration
    -> Maybe (NetworkCapacity DistillationCount)
    -> ProbabilisticAtomicAction (ListOutput BinaryOutput) DistillationCount
    -> WernerBellPairs
    -> MDP Double WernerBellPairs
executePAA pac mbCap act bps =
    if holdsWernerGuardTest (paaTest act) bps
       then mconcat
            [ computeListOutput pac mbCap (paaOutput act) (WernerBellPairs chosen) (WernerBellPairs rest')
            | Partial { chosen, rest = rest' } <- findElemsNDT staticBellPair (toList . paaInputBPs $ act) (unWernerBellPairs bps)
            ]
       else mempty

computeListOutput
    :: ProbabilisticActionConfiguration
    -> Maybe (NetworkCapacity DistillationCount)
    -> ListOutput BinaryOutput
    -> WernerBellPairs
    -> WernerBellPairs
    -> MDP Double WernerBellPairs
computeListOutput pac mbCap (ListOutput xs) chosen untouched =
    setAllCosts roundCost $ -- Set all costs to the round cost
                            -- Decohere pairs not selected for the combined action
        cmap (finalizeWernerPieces mbCap) $
                parallelCompose
        (go xs chosen)
        (fromDistribution (cmap retainedPieces (decohereState pac roundCost untouched)))
    where
    roundCost = combinedRoundCost (boOperation . snd) xs

    -- Decohere pairs inside chosen that ended up not being used for the primitive actions (i.e. those that are in restBps) for the difference between the round cost and the time taken by the local operations
    go [] restBps =
        fromDistribution (cmap retainedPieces (decohereState pac roundCost restBps))

    -- Takes a primitive output (i, out) from the combined list
    -- , finds all the ways of picking the required input pairs (i) from the input multiset (currentBps) 
    -- , for each of them computes the output of the primitive action (computePrimitiveOutput) 
    -- , recursively computing the output of the rest of the combined list (go outs (WernerBellPairs rest')) 
    -- , where rest' are the pairs that were not chosen for the primitive action
    go ((i, out):outs) (WernerBellPairs currentBps) =
        mconcat
            [ parallelCompose (computePrimitiveOutput pac roundCost out (WernerBellPairs chosenBps))
                              (go outs (WernerBellPairs rest'))
            | Partial { chosen = chosenBps, rest = rest' } <- findElemsNDT staticBellPair (toList . Mset.bellPairs $ i) currentBps
            ]

computePrimitiveOutput
    :: ProbabilisticActionConfiguration
    -> Int
    -> BinaryOutput
    -> WernerBellPairs
    -> MDP Double WernerPieces
computePrimitiveOutput _ _ BinaryOutput{boOperation = FSkip} _ =
    fromDistribution (cpure mempty)
computePrimitiveOutput _ _ BinaryOutput{boOperation = FDestroy} _ =
    fromDistribution (cpure mempty)
computePrimitiveOutput pac roundCost BinaryOutput{boOutputBP = outBp, boOperation = FCreate p w} chosen =
    requireCardinality "create" 0 chosen $
        cmap producedPieces $ createLike pac (fromRational p) w 1 roundCost outBp
computePrimitiveOutput pac roundCost BinaryOutput{boOutputBP = outBp, boOperation = FGenerate p w d} chosen =
    requireCardinality "generate" 0 chosen $
        cmap producedPieces $ createLike pac (fromRational p) w d roundCost outBp
computePrimitiveOutput pac roundCost BinaryOutput{boOutputBP = outBp, boOperation = FTransmit p _ d} chosen =
    requireCardinality "transmit" 1 chosen $
        cmap producedPieces $ transmitLike pac (fromRational p) d roundCost outBp chosen
computePrimitiveOutput pac roundCost BinaryOutput{boOutputBP = outBp, boOperation = FSwap p _ ds} chosen =
    requireCardinality "swap" 2 chosen $
        cmap producedPieces $ swapLike pac (fromRational p) (max (fst ds) (snd ds)) roundCost outBp chosen
computePrimitiveOutput pac roundCost BinaryOutput{boOutputBP = outBp, boOperation = FDistill _ d} chosen =
    requireCardinality "distill" 2 chosen $
        cmap producedPieces $ distillLike pac d roundCost outBp chosen

createLike
    :: ProbabilisticActionConfiguration
    -> Double
    -> Double
    -> Int
    -> Int
    -> TaggedBellPair DistillationCount
    -> MDP Double WernerBellPairs
createLike pac pSuccess w0 localCost roundCost outBp =
    fromDistribution . distributionFromOutcomes $
        [ (mempty, 1 - pSuccess)
        , (singletonMixed outBp, pSuccess * (1 - w0))
        ]
        <> 
        weightedOutcomes (pSuccess * w0) 
        -- pure pairs in the output are decohered 
        -- for the difference between the round cost and the time taken by the local operation
        (decohereState pac (remainingWait roundCost localCost) (singletonPure outBp))

transmitLike
    :: ProbabilisticActionConfiguration
    -> Double
    -> Int
    -> Int
    -> TaggedBellPair DistillationCount
    -> WernerBellPairs
    -> MDP Double WernerBellPairs
transmitLike pac pSuccess localCost roundCost outBp chosen =
    fromDistribution . distributionFromOutcomes $
        [ (mempty, 1 - pSuccess) ]
        <> case toList chosen of
            [TaggedBellPair _ inTag@WernerTag{wtStateKind}] ->
                let outCount = wtDistillations inTag
                 in case wtStateKind of
                    Pure ->
                        weightedOutcomes pSuccess $
                            decohereState pac (remainingWait roundCost localCost) (singletonPureWith outCount outBp)
                    Mixed ->
                        [(singletonMixedWith outCount outBp, pSuccess)]
            _ ->
                error "transmitLike: expected exactly one input Bell pair"

swapLike
    :: ProbabilisticActionConfiguration
    -> Double
    -> Int
    -> Int
    -> TaggedBellPair DistillationCount
    -> WernerBellPairs
    -> MDP Double WernerBellPairs
swapLike pac pSuccess _ roundCost outBp chosen =
    fromDistribution . distributionFromOutcomes $
        [ (mempty, 1 - pSuccess) ] <> successOutcomes
  where
    successOutcomes =
        case fmap bellPairTag (toList chosen) of
            [t1, t2] ->
                -- assign to the output pair the same number of distillations 
                -- as the more distilled input pair
                let outCount = max (wtDistillations t1) (wtDistillations t2)
                 in case (wtStateKind t1, wtStateKind t2) of
                    (Pure, Pure) ->
                        weightedOutcomes pSuccess $
                            -- pure pairs in the output are decohered for the whole round cost
                            -- as swap produces its output pair instantaneously at the start of the round
                            -- the rest of the round is classical communication or other operations
                            decohereState pac roundCost (singletonPureWith outCount outBp)
                    _ ->
                        [(singletonMixedWith outCount outBp, pSuccess)]
            _ ->
                error "swapLike: expected exactly two input Bell pairs"

distillLike
    :: ProbabilisticActionConfiguration
    -> Int
    -> Int
    -> TaggedBellPair DistillationCount
    -> WernerBellPairs
    -> MDP Double WernerBellPairs
distillLike pac _ roundCost outBp chosen =
    fromDistribution . distributionFromOutcomes $
        -- again, pure pairs in the output are decohered for the whole round cost
        case fmap bellPairTag (toList chosen) of
            [t1, t2] ->
                -- assign to the output pair a distillation count that is
                -- one more than the maximum distillation count of the input pairs
                let outCount = max (wtDistillations t1) (wtDistillations t2) + 1
                 in case (wtStateKind t1, wtStateKind t2) of
                    (Pure, Pure) ->
                        weightedOutcomes 1 $
                            decohereState pac roundCost (singletonPureWith outCount outBp)
                    (Pure, Mixed) ->
                        mixedPureOutcomes outCount
                    (Mixed, Pure) ->
                        mixedPureOutcomes outCount
                    (Mixed, Mixed) ->
                        [ (singletonMixedWith outCount outBp, 1 / 2)
                        , (mempty, 1 / 2)
                        ]
            _ ->
                error "distillLike: expected exactly two input Bell pairs"
  where
    mixedPureOutcomes outCount =
        weightedOutcomes (1 / 6) (decohereState pac roundCost (singletonPureWith outCount outBp))
            <> [ (singletonMixedWith outCount outBp, 1 / 3)
               , (mempty, 1 / 2)
               ]

decohereState
    :: ProbabilisticActionConfiguration
    -> Int
    -> WernerBellPairs
    -> D Double WernerBellPairs
decohereState _ deltaT bps | deltaT <= 0 = cpure bps
decohereState pac deltaT bps =
    cmap fmapToWerner (go (toList bps))
  where
    go [] = cpure []
    go (bp:bps') =
        cmap (\(x, xs) -> x : xs) (pair (decohereBellPair pac deltaT bp) (go bps'))

    fmapToWerner :: [TaggedBellPair WernerTag] -> WernerBellPairs
    fmapToWerner = WernerBellPairs . GHC.Exts.fromList

decohereBellPair
    :: ProbabilisticActionConfiguration
    -> Int
    -> TaggedBellPair WernerTag
    -> D Double (TaggedBellPair WernerTag)
decohereBellPair _ deltaT bp | deltaT <= 0 = cpure bp
decohereBellPair pac deltaT bp@(TaggedBellPair outBp tag@WernerTag{wtStateKind}) =
    case wtStateKind of
        Mixed ->
            cpure bp
        Pure ->
            let coherence = coherenceFactor pac outBp deltaT
             in if coherence >= 1 - epsilon
                   then cpure bp
                   else if coherence <= epsilon
                           then cpure (TaggedBellPair outBp tag{wtStateKind = Mixed})
                           else fromList
                                [ (bp, coherence)
                                , (TaggedBellPair outBp tag{wtStateKind = Mixed}, 1 - coherence)
                                ]

coherenceFactor :: ProbabilisticActionConfiguration -> BellPair -> Int -> Double
coherenceFactor pac bp deltaT =
    exp
        ( - fromIntegral deltaT / fromIntegral (coherenceTimeAt pac l1)
          - fromIntegral deltaT / fromIntegral (coherenceTimeAt pac l2)
        )
  where
    (l1, l2) = locations bp

coherenceTimeAt :: ProbabilisticActionConfiguration -> Location -> Int
coherenceTimeAt pac l =
    case pacCoherenceTime pac Map.!? l of
        Just t -> t
        Nothing -> error $ "no coherence time for " <> show l

singletonPure :: TaggedBellPair DistillationCount -> WernerBellPairs
singletonPure outBp = singletonPureWith (bellPairTag outBp) outBp

singletonMixed :: TaggedBellPair DistillationCount -> WernerBellPairs
singletonMixed outBp = singletonMixedWith (bellPairTag outBp) outBp

singletonPureWith :: DistillationCount -> TaggedBellPair DistillationCount -> WernerBellPairs
singletonPureWith count outBp = singletonWernerWith count Pure outBp

singletonMixedWith :: DistillationCount -> TaggedBellPair DistillationCount -> WernerBellPairs
singletonMixedWith count outBp = singletonWernerWith count Mixed outBp

singletonWernerWith :: DistillationCount -> StateKind -> TaggedBellPair DistillationCount -> WernerBellPairs
singletonWernerWith count kind outBp =
    WernerBellPairs . Mset.singleton' $
        TaggedBellPair (bellPair outBp) (WernerTag count kind)

distributionFromOutcomes :: [(WernerBellPairs, Double)] -> D Double WernerBellPairs
distributionFromOutcomes =
    fromList . fmap (second id) . filter ((> epsilon) . snd)

weightedOutcomes :: Double -> D Double WernerBellPairs -> [(WernerBellPairs, Double)]
weightedOutcomes weight =
    fmap (second (weight *)) . D.toListD

remainingWait :: Int -> Int -> Int
remainingWait roundCost localCost
    | localCost <= roundCost = roundCost - localCost
    | otherwise =
        error $ "remainingWait: local action cost " <> show localCost
             <> " exceeds round cost " <> show roundCost

epsilon :: Double
epsilon = 1e-12
