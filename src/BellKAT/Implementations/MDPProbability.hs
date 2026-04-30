{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Implementations.MDPProbability
    ( StaticBellPairs(..)
    , toStaticBellPairs
    , holdsStaticTest
    , StepCost(..)
    , MDP(..)
    , MDP'
    , minimizeStateSystem
    , execute
    , execute'
    , executeWith'
    ) where

import qualified GHC.Exts
import           GHC.Exts                    (toList)
import           Control.Subcategory.Functor
import           Control.Subcategory.Pointed

import qualified BellKAT.Utils.Multiset      as Mset
import           BellKAT.Utils.Choice
import           BellKAT.Utils.Distribution  (RationalOrDouble)
import           BellKAT.Definitions.Atomic
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Tests
import           BellKAT.Implementations.Configuration
import           BellKAT.Implementations.Output
import           BellKAT.Implementations.ProbAtomicOneStepQuantum (ProbAtomicOneStepPolicy)
import           BellKAT.Implementations.ProbabilisticQuantumOps (DistillationCount)
import           BellKAT.Implementations.QuantumOps
import           BellKAT.Utils.MDP

newtype StaticBellPairs = StaticBellPairs { unStaticBellPairs :: TaggedBellPairs DistillationCount }
    deriving stock (Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance GHC.Exts.IsList StaticBellPairs where
    type Item StaticBellPairs = TaggedBellPair DistillationCount
    fromList = StaticBellPairs . GHC.Exts.fromList
    toList = GHC.Exts.toList . Mset.bellPairs . unStaticBellPairs

instance Show StaticBellPairs where
    show = show . Mset.bellPairs . unStaticBellPairs

toStaticBellPairs :: RuntimeTag rTag DistillationCount => LabelledBellPairs cTag rTag -> StaticBellPairs
toStaticBellPairs = StaticBellPairs . staticBellPairs

holdsStaticTest :: Test test => test DistillationCount -> StaticBellPairs -> Bool
holdsStaticTest t = getBPsPredicate (toBPsPredicate t) . unStaticBellPairs

execute
    :: ProbAtomicOneStepPolicy (ListOutput QuantumOutput) DistillationCount
    -> StaticBellPairs
    -> MDP' StaticBellPairs
execute = execute'

execute'
    :: RationalOrDouble p
    => ProbAtomicOneStepPolicy (ListOutput QuantumOutput) DistillationCount
    -> StaticBellPairs
    -> MDP p StaticBellPairs
execute' policy bps =
    foldMap (\paa -> executePAA paa bps) (toList policy)

executeWith'
    :: RationalOrDouble p
    => ExecutionParams DistillationCount rTag cTag
    -> ProbAtomicOneStepPolicy (ListOutput QuantumOutput) DistillationCount
    -> StaticBellPairs
    -> MDP p StaticBellPairs
executeWith' ep policy = cmap (applyStaticExecutionParams ep) . execute' policy

applyStaticExecutionParams :: ExecutionParams DistillationCount rTag cTag -> StaticBellPairs -> StaticBellPairs
applyStaticExecutionParams EP{epNetworkCapacity = Nothing} = id
applyStaticExecutionParams EP{epNetworkCapacity = Just cap} =
    StaticBellPairs . fixNetworkCapacity cap . unStaticBellPairs

executePAA
    :: RationalOrDouble p
    => ProbabilisticAtomicAction (ListOutput QuantumOutput) DistillationCount
    -> StaticBellPairs
    -> MDP p StaticBellPairs
executePAA act bps =
    if holdsStaticTest (paaTest act) bps
       then mconcat
            [ computeListOutput (paaOutput act) (StaticBellPairs chosen) (StaticBellPairs rest')
            | Partial { chosen, rest = rest' } <- findElemsNDT id (toList . paaInputBPs $ act) (unStaticBellPairs bps)
            ]
       else mempty

computeListOutput
    :: RationalOrDouble p
    => ListOutput QuantumOutput
    -> StaticBellPairs
    -> StaticBellPairs
    -> MDP p StaticBellPairs
computeListOutput (ListOutput xs) chosen untouched =
    parallelCompose (go xs chosen) (cpure untouched)
  where
    go [] restBps = cpure restBps
    go ((i, out):outs) (StaticBellPairs currentBps) =
        mconcat
            [ parallelCompose (computePrimitiveOutput out (StaticBellPairs chosenBps))
                              (go outs (StaticBellPairs rest'))
            | Partial { chosen = chosenBps, rest = rest' } <- findElemsNDT id (toList . Mset.bellPairs $ i) currentBps
            ]

computePrimitiveOutput
    :: RationalOrDouble p
    => QuantumOutput
    -> StaticBellPairs
    -> MDP p StaticBellPairs
computePrimitiveOutput QuantumOutput{qoOperation = FSkip} _ =
    singleGenerator (primitiveCost FSkip) [mempty]
computePrimitiveOutput QuantumOutput{qoOperation = FDestroy} _ =
    singleGenerator (primitiveCost FDestroy) [mempty]
computePrimitiveOutput QuantumOutput{qoOutputBP = outBp, qoOperation = op@(FCreate p _)} chosen =
    requireCardinality "create" 0 chosen $
        successOrFailure (primitiveCost op) p (StaticBellPairs $ Mset.singleton' outBp)
computePrimitiveOutput QuantumOutput{qoOutputBP = outBp, qoOperation = op@(FGenerate p _ _)} chosen =
    requireCardinality "generate" 0 chosen $
        successOrFailure (primitiveCost op) p (StaticBellPairs $ Mset.singleton' outBp)
computePrimitiveOutput QuantumOutput{qoOutputBP = outBp, qoOperation = op@(FTransmit p _ _)} chosen =
    requireCardinality "transmit" 1 chosen $
        successOrFailure (primitiveCost op) p (StaticBellPairs . Mset.singleton' $ outBpWithInputCount outBp chosen)
computePrimitiveOutput QuantumOutput{qoOutputBP = outBp, qoOperation = op@(FSwap p _ _)} chosen =
    requireCardinality "swap" 2 chosen $
        successOrFailure (primitiveCost op) p (StaticBellPairs . Mset.singleton' $ outBpWithMaxInputCount outBp chosen)
computePrimitiveOutput QuantumOutput{qoOutputBP = outBp, qoOperation = op@(FSimSwap p _ distanceSpecs)} chosen =
    requireCardinality "simultaneous swap" (length distanceSpecs) chosen $
        successOrFailure (primitiveCost op) p (StaticBellPairs . Mset.singleton' $ outBpWithMaxInputCountN outBp chosen)
computePrimitiveOutput QuantumOutput{qoOperation = op@(FDistill _ _)} chosen =
    requireCardinality "distill" 2 chosen $
        error $
            "executeMDPWith': FDistill is not supported in the static MDP yet; "
            <> "its success probability depends on runtime Werner tags that are erased here"
            <> " (cost would have been " <> show (primitiveCost op) <> ")"

outBpWithInputCount :: TaggedBellPair DistillationCount -> StaticBellPairs -> TaggedBellPair DistillationCount
outBpWithInputCount outBp chosen =
    case toList chosen of
        [TaggedBellPair _ count] -> outBp { bellPairTag = count }
        _ -> error "outBpWithInputCount: expected exactly one input Bell pair"

outBpWithMaxInputCount :: TaggedBellPair DistillationCount -> StaticBellPairs -> TaggedBellPair DistillationCount
outBpWithMaxInputCount outBp chosen =
    case fmap bellPairTag (toList chosen) of
        [count1, count2] -> outBp { bellPairTag = max count1 count2 }
        _ -> error "outBpWithMaxInputCount: expected exactly two input Bell pairs"

outBpWithMaxInputCountN :: TaggedBellPair DistillationCount -> StaticBellPairs -> TaggedBellPair DistillationCount
outBpWithMaxInputCountN outBp chosen =
    case fmap bellPairTag (toList chosen) of
        [] -> error "outBpWithMaxInputCountN: expected at least one input Bell pair"
        counts -> outBp { bellPairTag = maximum counts }
