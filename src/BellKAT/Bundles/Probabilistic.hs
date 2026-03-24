{-# LANGUAGE OverloadedStrings #-}
module BellKAT.Bundles.Probabilistic where

import           Data.Default

import           BellKAT.Definitions.Structures
import           BellKAT.Definitions.Policy
import           BellKAT.Definitions.Tests
import           BellKAT.Definitions.Core
import           BellKAT.Utils.Distribution
import           BellKAT.Utils.Convex

import           BellKAT.ActionEmbeddings
import           BellKAT.PolicyEmbeddings
import           BellKAT.Bundles.Core
import           BellKAT.Bundles.Desugaring

import           BellKAT.Implementations.Configuration
import qualified BellKAT.Implementations.GuardedAutomataStepQuantum    as GASQ
import qualified BellKAT.Implementations.ProbAtomicOneStepQuantum    as PAOSQ

-- | builds an automaton t`BellKAT.Utils.Automata.Guarded.GuardedFA` from a guarded policy 
-- `OrderedGuardedPolicy` using probabilistic interpretation configured via 
-- `ProbabilisticActionConfiguration` guided by `GASQ.GuardedAutomatonStepQuantum` with 
-- `PAOSQ.ProbAtomicOneStepPolicy` as an action.
probabilisticAutomatonStage
    :: forall tag. (DDom tag, Default tag)
    => forall test. (DecidableBoolean (test tag), Show (test tag))
    => Stage ()
        (OrderedGuardedPolicy (test tag) (CreateBellPairArgs' tag))
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy' tag))
probabilisticAutomatonStage = Stage
    { stageName = "constructing_guarded_automaton_prob"
    , stageConfig = ()
    , stageFunction = \() -> meaning
    }

guardedAutomatonStage
    :: forall tag. (DDom tag, Default tag)
    => forall test. (Test test, DecidableBoolean (test tag))
    => ExecutionParams tag tag ()
    -> TaggedBellPairs tag 
    -> Stage (ExecutionParams tag tag (), TaggedBellPairs tag)
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy' tag))
        (CD' (TaggedBellPairs tag))
guardedAutomatonStage ep initialState = Stage
    { stageName = "executing_guarded_automaton_prob"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.execute (getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith ep') gfa
        initialState'
    }

guardedAutomatonStage'
    :: forall p. RationalOrDouble p
    => forall tag. (DDom tag, Default tag)
    => forall test. (Test test, DecidableBoolean (test tag))
    => ExecutionParams tag tag ()
    -> TaggedBellPairs tag
    -> Stage (ExecutionParams tag tag (), TaggedBellPairs tag)
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy' tag))
        (CD p (TaggedBellPairs tag))
guardedAutomatonStage' ep initialState = Stage
    { stageName = "executing_guarded_automaton_prob'"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.execute (getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith' ep') gfa
        initialState'
    }

guardedToSystemStage'
    :: forall p. RationalOrDouble p
    => forall tag. (DDom tag, Default tag)
    => forall test. (Test test, DecidableBoolean (test tag))
    => ExecutionParams tag tag ()
    -> TaggedBellPairs tag 
    -> Stage (ExecutionParams tag tag (), TaggedBellPairs tag)
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy' tag))
        (GASQ.StateSystem (CD p) (TaggedBellPairs tag))
guardedToSystemStage' ep initialState = Stage
    { stageName = "system_stage_prob'"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.executeSystem (getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith' ep') gfa
        initialState'
    }

guardedToSystemStage
    :: forall tag. (DDom tag, Default tag)
    => forall test. (Test test, DecidableBoolean (test tag))
    => ExecutionParams tag tag ()
    -> TaggedBellPairs tag 
    -> Stage (ExecutionParams tag tag (), TaggedBellPairs tag)
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy' tag))
        (GASQ.StateSystem CD' (TaggedBellPairs tag))
guardedToSystemStage ep initialState = Stage
    { stageName = "system_stage_prob"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.executeSystem (getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith ep') gfa
        initialState'
    }

guardedToStatesStage
    :: forall tag. (DDom tag, Default tag)
    => forall test. (Test test, DecidableBoolean (test tag))
    => ExecutionParams tag tag ()
    -> TaggedBellPairs tag
    -> Stage (ExecutionParams tag tag (), TaggedBellPairs tag)
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy' tag))
        (GASQ.ComputedState CD' (TaggedBellPairs tag))
guardedToStatesStage ep initialState = Stage
    { stageName = "guarded_states_stage_prob"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.executeState (getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith ep') gfa
        initialState'
    }

-- | builds an automaton t`BellKAT.Utils.Automata.Guarded.GuardedFA` from a guarded policy `OrderedGuardedPolicy` using probabilistic interpretation configured via `ProbabilisticActionConfiguration` guided by `GASQ.GuardedAutomatonStepQuantum` with `PAOSQ.ProbAtomicOneStepPolicy` as an action.
probStarPolicyAutomatonPipeline
    :: forall tag. (DDom tag, Default tag)
    => forall test. (DecidableBoolean (test tag), Show (test tag))
    => ProbabilisticActionConfiguration
    -> Pipeline (Simple (OrderedGuardedPolicy (test tag)) tag)
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy' tag))
probStarPolicyAutomatonPipeline pac
    = stage (probabilisticDesugarStage pac) >>> stage probabilisticAutomatonStage

probStarPolicyPipeline'
    :: forall p. RationalOrDouble p
    => forall tag. (DDom tag, Default tag)
    => forall test. (Test test, DecidableBoolean (test tag), Show (test tag))
    => ProbabilisticActionConfiguration
    -> ExecutionParams tag tag ()
    -> TaggedBellPairs tag 
    -> Pipeline (Simple (OrderedGuardedPolicy (test tag)) tag) (CD p (TaggedBellPairs tag))
probStarPolicyPipeline' pac ep initialState = 
    probStarPolicyAutomatonPipeline pac >>> stage (guardedAutomatonStage' ep initialState)

probStarPolicySystemPipeline'
    :: forall p. RationalOrDouble p
    => forall tag. (DDom tag, Default tag)
    => forall test. (Test test, DecidableBoolean (test tag), Show (test tag))
    => ProbabilisticActionConfiguration
    -> ExecutionParams tag tag ()
    -> TaggedBellPairs tag 
    -> Pipeline (Simple (OrderedGuardedPolicy (test tag)) tag) (GASQ.StateSystem (CD p) (TaggedBellPairs tag))
probStarPolicySystemPipeline' pac ep initialState = 
    probStarPolicyAutomatonPipeline pac >>> stage (guardedToSystemStage' ep initialState)

applyProbStarPolicy 
    :: forall tag. (DDom tag, Default tag)
    => forall test. (Test test, DecidableBoolean (test tag), Show (test tag))
    => ProbabilisticActionConfiguration 
    -> ExecutionParams tag tag ()
    -> Simple (OrderedGuardedPolicy (test tag)) tag 
    -> TaggedBellPairs tag -> CD' (TaggedBellPairs tag)
applyProbStarPolicy = applyProbStarPolicy'

applyProbStarPolicy' 
    :: forall p. RationalOrDouble p
    => forall tag. (DDom tag, Default tag)
    => forall test. (Test test, DecidableBoolean (test tag), Show (test tag))
    => ProbabilisticActionConfiguration 
    -> ExecutionParams tag tag ()
    -> Simple (OrderedGuardedPolicy (test tag)) tag 
    -> TaggedBellPairs tag -> CD p (TaggedBellPairs tag)
applyProbStarPolicy' pac ep policy initialState = 
    runNonLoggedPipeline (probStarPolicyPipeline' pac ep initialState) policy

applyProbStarPolicySystem
    :: forall tag. (DDom tag, Default tag)
    => forall test. (Test test, DecidableBoolean (test tag), Show (test tag))
    => ProbabilisticActionConfiguration 
    -> ExecutionParams tag tag ()
    -> Simple (OrderedGuardedPolicy (test tag)) tag 
    -> TaggedBellPairs tag -> GASQ.StateSystem CD' (TaggedBellPairs tag)
applyProbStarPolicySystem pac ep policy initialState= 
    runNonLoggedPipeline (probStarPolicySystemPipeline' pac ep initialState) policy
