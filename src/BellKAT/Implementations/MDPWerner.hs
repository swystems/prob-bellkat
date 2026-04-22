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
    , StateKind(..)
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

-- | Wrapper for pairs with purity tag
newtype WernerBellPairs = WernerBellPairs { unWernerBellPairs :: TaggedBellPairs StateKind }
    deriving stock (Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance GHC.Exts.IsList WernerBellPairs where
    type Item WernerBellPairs = TaggedBellPair StateKind
    fromList = WernerBellPairs . GHC.Exts.fromList
    toList = GHC.Exts.toList . Mset.bellPairs . unWernerBellPairs

instance Show WernerBellPairs where
    show (WernerBellPairs bps) =
        "⦃" <> renderPairs (toList (Mset.bellPairs bps)) <> "⦄"
      where
        renderPairs [] = ""
        renderPairs [x] = renderPair x
        renderPairs (x:xs) = renderPair x <> "," <> renderPairs xs

        renderPair :: TaggedBellPair StateKind -> String
        renderPair (TaggedBellPair bp kind) =
            let (l1, l2) = locations bp
                sep = case kind of
                    Pure -> "-"
                    Mixed -> "="
             in name l1 <> sep <> name l2

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

finalizeWernerPieces :: Maybe (NetworkCapacity ()) -> WernerPieces -> WernerBellPairs
finalizeWernerPieces Nothing (WernerPieces produced retained) =
    produced <> retained
finalizeWernerPieces (Just cap) (WernerPieces produced retained) =
    WernerBellPairs . (Mset.@ ()) . Mset.fromList $ producedKept <> retainedKept
  where
    (producedKept, remainingCounts) =
        takeWernerCapacity (capacityCounts cap) (toList produced)
    (retainedKept, _) =
        takeWernerCapacity remainingCounts (toList retained)

capacityCounts :: NetworkCapacity () -> Map.Map BellPair Int
capacityCounts (NC cap) =
    Map.fromListWith (+) [ (bellPair tbp, 1) | tbp <- toList cap ]

takeWernerCapacity
    :: Map.Map BellPair Int
    -> [TaggedBellPair StateKind]
    -> ([TaggedBellPair StateKind], Map.Map BellPair Int)
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

toWernerBellPairs :: LabelledBellPairs cTag StateKind -> WernerBellPairs
toWernerBellPairs = WernerBellPairs . Mset.map' id

-- | Evaluates test also on the purity tags of the pairs
holdsWernerTest :: KindedTest () -> WernerBellPairs -> Bool
holdsWernerTest t = getBPsPredicate (toSelectorPredicate t) . expandSelectorState . unWernerBellPairs

-- | Evaluates test ignoring the purity tags
holdsWernerGuardTest :: Test test => test () -> WernerBellPairs -> Bool
holdsWernerGuardTest t = getBPsPredicate (toBPsPredicate t) . staticBellPairs . unWernerBellPairs

-- | Expands into a selector that includes both the static and dynamic versions of each pair
expandSelectorState :: TaggedBellPairs StateKind -> TaggedBellPairs PairSelector
expandSelectorState (Mset.LMS (bps, ())) =
    Mset.fromList (concatMap expandPair (GHC.Exts.toList bps)) Mset.@ ()
  where
    expandPair (TaggedBellPair bp kind) =
        [ TaggedBellPair bp StaticPair
        , TaggedBellPair bp $
            case kind of
                Pure -> PurePair
                Mixed -> MixedPair
        ]

execute
    :: ProbabilisticActionConfiguration
    -> ProbAtomicOneStepPolicy (ListOutput BinaryOutput) ()
    -> WernerBellPairs
    -> MDP' WernerBellPairs
execute = execute'

execute'
    :: RationalOrDouble p
    => ProbabilisticActionConfiguration
    -> ProbAtomicOneStepPolicy (ListOutput BinaryOutput) ()
    -> WernerBellPairs
    -> MDP p WernerBellPairs
execute' pac policy =
    mapMDPProbabilities realToFrac . executeDouble pac Nothing policy

executeWith'
    :: RationalOrDouble p
    => ProbabilisticActionConfiguration
    -> ExecutionParams () rTag cTag
    -> ProbAtomicOneStepPolicy (ListOutput BinaryOutput) ()
    -> WernerBellPairs
    -> MDP p WernerBellPairs
executeWith' pac ep policy =
    mapMDPProbabilities realToFrac . executeDouble pac (epNetworkCapacity ep) policy

executeDouble
    :: ProbabilisticActionConfiguration
-> Maybe (NetworkCapacity ())
    -> ProbAtomicOneStepPolicy (ListOutput BinaryOutput) ()
    -> WernerBellPairs
    -> MDP Double WernerBellPairs
executeDouble pac mbCap policy bps =
    foldMap (\paa -> executePAA pac mbCap paa bps) (toList policy)

executePAA
    :: ProbabilisticActionConfiguration
-> Maybe (NetworkCapacity ())
    -> ProbabilisticAtomicAction (ListOutput BinaryOutput) ()
    -> WernerBellPairs
    -> MDP Double WernerBellPairs
executePAA pac mbCap act bps =
    if holdsWernerStaticTest (paaTest act) bps
       then mconcat
            [ computeListOutput pac mbCap (paaOutput act) (WernerBellPairs chosen) (WernerBellPairs rest')
            | Partial { chosen, rest = rest' } <- findElemsNDT staticBellPair (toList . paaInputBPs $ act) (unWernerBellPairs bps)
            ]
       else mempty

computeListOutput
    :: ProbabilisticActionConfiguration
-> Maybe (NetworkCapacity ())
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
    -> MDP Double WernerBellPairs
computePrimitiveOutput _ _ BinaryOutput{boOperation = FSkip} _ =
    fromDistribution (cpure mempty)
computePrimitiveOutput _ _ BinaryOutput{boOperation = FDestroy} _ =
    fromDistribution (cpure mempty)
computePrimitiveOutput pac roundCost BinaryOutput{boOutputBP = outBp, boOperation = FCreate p w} chosen =
    requireCardinality "create" 0 chosen $
        createLike pac (fromRational p) w 1 roundCost outBp
computePrimitiveOutput pac roundCost BinaryOutput{boOutputBP = outBp, boOperation = FGenerate p w d} chosen =
    requireCardinality "generate" 0 chosen $
        createLike pac (fromRational p) w d roundCost outBp
computePrimitiveOutput pac roundCost BinaryOutput{boOutputBP = outBp, boOperation = FTransmit p _ d} chosen =
    requireCardinality "transmit" 1 chosen $
        transmitLike pac (fromRational p) d roundCost outBp chosen
computePrimitiveOutput pac roundCost BinaryOutput{boOutputBP = outBp, boOperation = FSwap p _ ds} chosen =
    requireCardinality "swap" 2 chosen $
        swapLike pac (fromRational p) (max (fst ds) (snd ds)) roundCost outBp chosen
computePrimitiveOutput pac roundCost BinaryOutput{boOutputBP = outBp, boOperation = FDistill _ d} chosen =
    requireCardinality "distill" 2 chosen $
        distillLike pac d roundCost outBp chosen

createLike
    :: ProbabilisticActionConfiguration
    -> Double
    -> Double
    -> Int
    -> Int
    -> TaggedBellPair ()
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
    -> TaggedBellPair ()
    -> WernerBellPairs
    -> MDP Double WernerBellPairs
transmitLike pac pSuccess localCost roundCost outBp chosen =
    fromDistribution . distributionFromOutcomes $
        [ (mempty, 1 - pSuccess) ]
        <> case toList chosen of
            [TaggedBellPair _ kind] ->
                case kind of
                    Pure ->
                        weightedOutcomes pSuccess $
                            decohereState pac (remainingWait roundCost localCost) (singletonPure outBp)
                    Mixed ->
                        [(singletonMixed outBp, pSuccess)]
            _ ->
                error "transmitLike: expected exactly one input Bell pair"

swapLike
    :: ProbabilisticActionConfiguration
    -> Double
    -> Int
    -> Int
    -> TaggedBellPair ()
    -> WernerBellPairs
    -> MDP Double WernerBellPairs
swapLike pac pSuccess _ roundCost outBp chosen =
    fromDistribution . distributionFromOutcomes $
        [ (mempty, 1 - pSuccess) ] <> successOutcomes
  where
    successOutcomes =
        case fmap bellPairTag (toList chosen) of
            [Pure, Pure] ->
                weightedOutcomes pSuccess $
                    -- pure pairs in the output are decohered for the whole round cost
                    -- as swap produces its output pair instantaneously at the start of the round
                    -- the rest of the round is classical communication or other operations
                    decohereState pac roundCost (singletonPure outBp)
            [_, _] ->
                [(singletonMixed outBp, pSuccess)]
            _ ->
                error "swapLike: expected exactly two input Bell pairs"

distillLike
    :: ProbabilisticActionConfiguration
    -> Int
    -> Int
    -> TaggedBellPair ()
    -> WernerBellPairs
    -> MDP Double WernerBellPairs
distillLike pac _ roundCost outBp chosen =
    fromDistribution . distributionFromOutcomes $
        -- again, pure pairs in the output are decohered for the whole round cost
        case fmap bellPairTag (toList chosen) of
            [Pure, Pure] ->
                weightedOutcomes 1 $
                    decohereState pac roundCost (singletonPure outBp)
            [Pure, Mixed] ->
                mixedPureOutcomes
            [Mixed, Pure] ->
                mixedPureOutcomes
            [Mixed, Mixed] ->
                [ (singletonMixed outBp, 1 / 2)
                , (mempty, 1 / 2)
                ]
            _ ->
                error "distillLike: expected exactly two input Bell pairs"
  where
    mixedPureOutcomes =
        weightedOutcomes (1 / 6) (decohereState pac roundCost (singletonPure outBp))
            <> [ (singletonMixed outBp, 1 / 3)
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

    fmapToWerner :: [TaggedBellPair StateKind] -> WernerBellPairs
    fmapToWerner = WernerBellPairs . GHC.Exts.fromList

decohereBellPair
    :: ProbabilisticActionConfiguration
    -> Int
    -> TaggedBellPair StateKind
    -> D Double (TaggedBellPair StateKind)
decohereBellPair _ deltaT bp | deltaT <= 0 = cpure bp
decohereBellPair pac deltaT bp@(TaggedBellPair outBp kind) =
    case kind of
        Mixed ->
            cpure bp
        Pure ->
            let coherence = coherenceFactor pac outBp deltaT
             in if coherence >= 1 - epsilon
                   then cpure bp
                   else if coherence <= epsilon
                           then cpure (TaggedBellPair outBp Mixed)
                           else fromList
                                [ (bp, coherence)
                                , (TaggedBellPair outBp Mixed, 1 - coherence)
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

singletonPure :: TaggedBellPair () -> WernerBellPairs
singletonPure outBp = WernerBellPairs . Mset.singleton' $ fmap (const Pure) outBp

singletonMixed :: TaggedBellPair () -> WernerBellPairs
singletonMixed outBp = WernerBellPairs . Mset.singleton' $ fmap (const Mixed) outBp

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
