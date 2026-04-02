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

import           Data.Bifunctor              (first, second)
import           Data.List                   (intercalate)
import           Data.Monoid                 (Sum (..))
import qualified Data.IntMap.Strict          as IM
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import qualified GHC.Exts
import           GHC.Exts                    (fromList, toList)
import           Control.Subcategory.Bind
import           Control.Subcategory.Functor
import           Control.Subcategory.Pointed

import qualified BellKAT.Utils.Multiset      as Mset
import           BellKAT.Utils.Choice
import           BellKAT.Utils.Convex
import           BellKAT.Utils.Convex.Constraint
import           BellKAT.Utils.Distribution  (D, RationalOrDouble)
import qualified BellKAT.Utils.Distribution  as D
import           BellKAT.Definitions.Atomic
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Tests
import           BellKAT.Implementations.Configuration
import           BellKAT.Implementations.Output
import           BellKAT.Implementations.ProbAtomicOneStepQuantum (ProbAtomicOneStepPolicy)
import           BellKAT.Implementations.QuantumOps
import           BellKAT.Utils.Automata.Transitions.Functorial (StateSystem(..))

newtype StaticBellPairs = StaticBellPairs { unStaticBellPairs :: TaggedBellPairs () }
    deriving stock (Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance GHC.Exts.IsList StaticBellPairs where
    type Item StaticBellPairs = TaggedBellPair ()
    fromList = StaticBellPairs . GHC.Exts.fromList
    toList = GHC.Exts.toList . Mset.bellPairs . unStaticBellPairs

instance Show StaticBellPairs where
    show = show . Mset.bellPairs . unStaticBellPairs

toStaticBellPairs :: RuntimeTag rTag () => LabelledBellPairs cTag rTag -> StaticBellPairs
toStaticBellPairs = StaticBellPairs . staticBellPairs

holdsStaticTest :: Test test => test () -> StaticBellPairs -> Bool
holdsStaticTest t = getBPsPredicate (toBPsPredicate t) . unStaticBellPairs

newtype StepCost = StepCost { getStepCost :: Sum Int }
    deriving stock (Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance Show StepCost where
    show (StepCost c) = show (getSum c)

mkStepCost :: Int -> StepCost
mkStepCost = StepCost . Sum

newtype MDP p a = MDP { unMDP :: CD p (a, StepCost) }
    deriving newtype (Eq, Ord, Semigroup, Monoid)

type MDP' = MDP Probability

instance Foldable (MDP p) where
    foldMap f = foldMap (f . fst) . unMDP

instance Constrained (MDP p) where
    type Dom (MDP p) a = (DDom a, DDom (a, StepCost))

instance RationalOrDouble p => CPointed (MDP p) where
    cpure = MDP . cpure . \x -> (x, mempty)

instance RationalOrDouble p => CFunctor (MDP p) where
    cmap f = MDP . cmap (first f) . unMDP

instance RationalOrDouble p => CBind (MDP p) where
    cjoin = MDP . cjoin . cmap (\(MDP ma, c) -> cmap (second (c <>)) ma) . unMDP

instance (Show p, Show a) => Show (MDP p a) where
    show (MDP mdp)
        | null gens = "⦅⦆"
        | otherwise = "⦅ " <> intercalate ", " (showGenerator <$> gens) <> " ⦆"
      where
        gens = getGenerators mdp

        showGenerator :: D p (a, StepCost) -> String
        showGenerator = intercalate "+" . fmap showOutcome . D.toListD

        showOutcome ((x, c), p) =
            show x <> "×《" <> show p <> ", " <> show c <> "》"

minimizeStateSystem
    :: (Eq p, Num p, RationalOrDouble p)
    => StateSystem (MDP p) StaticBellPairs
    -> StateSystem (MDP p) StaticBellPairs
minimizeStateSystem ss = SS
    { ssInitial = resolveNode (ssInitial ss)
    , ssTransitions = IM.fromListWith Map.union
        [ (pc, Map.singleton bps (cmap resolveNode outgoing))
        | (pc, perState) <- IM.toList (ssTransitions ss)
        , (bps, outgoing) <- Map.toList perState
        , let node = (pc, bps)
        , resolveNode node == node
        ]
    }
  where
    resolveNode node = go Set.empty node
      where
        go seen current
            | current `Set.member` seen = current
            | otherwise =
                case silentSuccessor current of
                    Just next | next /= current -> go (Set.insert current seen) next
                    _ -> current

    silentSuccessor (pc, bps) = do
        perState <- IM.lookup pc (ssTransitions ss)
        mdp <- Map.lookup bps perState
        deterministicZeroCostSuccessor mdp

deterministicZeroCostSuccessor :: (Eq p, Num p) => MDP p (Int, StaticBellPairs) -> Maybe (Int, StaticBellPairs)
deterministicZeroCostSuccessor (MDP mdp) = do
    [generator] <- pure $ getGenerators mdp
    [((next, cost), prob)] <- pure $ D.toListD generator
    if prob == 1 && cost == mempty
       then Just next
       else Nothing

execute
    :: ProbAtomicOneStepPolicy (ListOutput QuantumOutput) ()
    -> StaticBellPairs
    -> MDP' StaticBellPairs
execute = execute'

execute'
    :: RationalOrDouble p
    => ProbAtomicOneStepPolicy (ListOutput QuantumOutput) ()
    -> StaticBellPairs
    -> MDP p StaticBellPairs
execute' policy bps =
    foldMap (\paa -> executePAA paa bps) (toList policy)

executeWith'
    :: RationalOrDouble p
    => ExecutionParams () rTag cTag
    -> ProbAtomicOneStepPolicy (ListOutput QuantumOutput) ()
    -> StaticBellPairs
    -> MDP p StaticBellPairs
executeWith' ep policy = cmap (applyStaticExecutionParams ep) . execute' policy

applyStaticExecutionParams :: ExecutionParams () rTag cTag -> StaticBellPairs -> StaticBellPairs
applyStaticExecutionParams EP{epNetworkCapacity = Nothing} = id
applyStaticExecutionParams EP{epNetworkCapacity = Just cap} =
    StaticBellPairs . fixNetworkCapacity cap . unStaticBellPairs

executePAA
    :: RationalOrDouble p
    => ProbabilisticAtomicAction (ListOutput QuantumOutput) ()
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
    singleGenerator 0 [mempty]
computePrimitiveOutput QuantumOutput{qoOperation = FDestroy} _ =
    singleGenerator 0 [mempty]
computePrimitiveOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FCreate p _} chosen =
    requireCardinality "create" 0 chosen $
        successOrFailure 1 p (StaticBellPairs $ Mset.singleton' outBp)
computePrimitiveOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FGenerate p _ d} chosen =
    requireCardinality "generate" 0 chosen $
        successOrFailure d p (StaticBellPairs $ Mset.singleton' outBp)
computePrimitiveOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FTransmit p _ d} chosen =
    requireCardinality "transmit" 1 chosen $
        successOrFailure d p (StaticBellPairs $ Mset.singleton' outBp)
computePrimitiveOutput QuantumOutput{qoOutputBP = outBp, qoOperation = FSwap p _ (d1, d2)} chosen =
    requireCardinality "swap" 2 chosen $
        successOrFailure (max d1 d2) p (StaticBellPairs $ Mset.singleton' outBp)
computePrimitiveOutput QuantumOutput{qoOperation = FDistill _ d} chosen =
    requireCardinality "distill" 2 chosen $
        error $
            "executeMDPWith': FDistill is not supported in the static MDP yet; "
            <> "its success probability depends on runtime Werner tags that are erased here"
            <> " (cost would have been " <> show d <> ")"

requireCardinality :: String -> Int -> StaticBellPairs -> a -> a
requireCardinality opName expected bps result
    | length (toList bps) == expected = result
    | otherwise =
        error $
            "computePrimitiveOutput: " <> opName <> " expected "
            <> show expected <> " inputs, got " <> show (length (toList bps))

singleGenerator :: RationalOrDouble p => Int -> [StaticBellPairs] -> MDP p StaticBellPairs
singleGenerator cost states =
    fromGenerator
        [ ((st, mkStepCost cost), 1)
        | st <- states
        ]

successOrFailure :: RationalOrDouble p => Int -> Rational -> StaticBellPairs -> MDP p StaticBellPairs
successOrFailure cost p successState
    | p == 0 = singleGenerator cost [mempty]
    | p == 1 = singleGenerator cost [successState]
    | otherwise =
        fromGenerator
            [ ((successState, mkStepCost cost), fromRational p)
            , ((mempty,       mkStepCost cost), fromRational (1 - p))
            ]

fromGenerator :: (RationalOrDouble p, DDom a, DDom (a, StepCost)) => [((a, StepCost), p)] -> MDP p a
fromGenerator = MDP . fromList . pure . fromList

parallelCompose
    :: RationalOrDouble p
    => MDP p StaticBellPairs
    -> MDP p StaticBellPairs
    -> MDP p StaticBellPairs
parallelCompose (MDP lhs) (MDP rhs) =
    MDP . fromList $
        [ fromList
            [ ((s1 <> s2, max c1 c2), p1 * p2)
            | ((s1, c1), p1) <- D.toListD d1
            , ((s2, c2), p2) <- D.toListD d2
            ]
        | d1 <- getGenerators lhs
        , d2 <- getGenerators rhs
        ]
