{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module BellKAT.Bundles.OpBased where

import           Data.Default
import           Data.Typeable

import           BellKAT.Definitions.Structures
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Policy
import           BellKAT.Definitions.Tests
import           BellKAT.Utils.Convex
import           BellKAT.Utils.Distribution

import           BellKAT.ActionEmbeddings
import           BellKAT.PolicyEmbeddings
import           BellKAT.Implementations.Output
import           BellKAT.Implementations.Configuration (ExecutionParams)
import           BellKAT.Implementations.MDPProbability (MDP, StaticBellPairs)
import qualified BellKAT.Implementations.MDPProbability as MDPP
import           BellKAT.Implementations.MDPWerner (WernerBellPairs)
import qualified BellKAT.Implementations.MDPWerner as MDPW
import           BellKAT.Implementations.ProbabilisticQuantumOps (BinaryOutput, DistillationCount)
import           BellKAT.Implementations.QuantumOps (QuantumOutput, QuantumTag, MaxClock)
import           BellKAT.Bundles.Core
import           BellKAT.Bundles.Desugaring (probabilisticOpDesugarStage)

import qualified BellKAT.Implementations.GuardedAutomataStepQuantum    as GASQ
import qualified BellKAT.Implementations.ProbAtomicOneStepQuantum    as PAOSQ

probabilisticOpAutomatonStage
    :: (OpOutput output Op, Monoid output, Show output)
    => Default (STag output)
    => (DecidableBoolean (test (STag output)), Show (test (STag output)))
    => Stage ()
        (OrderedGuardedPolicy (test (STag output)) (CreateBellPairArgs Op (STag output)))
        (GASQ.GuardedAutomatonStepQuantum (test (STag output)) (PAOSQ.ProbAtomicOneStepPolicy output (STag output)))
probabilisticOpAutomatonStage = Stage
    { stageName = "constructing_guarded_op_automaton"
    , stageConfig = ()
    , stageFunction = \() -> meaning
    }

guardedAutomatonStage
    :: OpOutput output Op
    => Semigroup (CTag output)
    => (Test test, DecidableBoolean (test (STag output)))
    => ExecutionParams (STag output) (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Stage (ExecutionParams (STag output) (RTag output) (CTag output), OutputBellPairs output)
        (GASQ.GuardedAutomatonStepQuantum (test (STag output)) (PAOSQ.ProbAtomicOneStepPolicy output (STag output)))
        (OutputM output (OutputBellPairs output))
guardedAutomatonStage ep initialState = Stage
    { stageName = "executing_guarded_op_automaton"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.execute ((. staticBellPairs) . getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith ep') gfa
        initialState'
    }

guardedAutomatonStage'
    :: forall p. RationalOrDouble p
    => forall output. (OpOutput output Op, OutputM output ~ CD')
    => (DDom (RTag output), Semigroup (CTag output))
    => forall test. (Test test, DecidableBoolean (test (STag output)))
    => ExecutionParams (STag output) (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Stage (ExecutionParams (STag output) (RTag output) (CTag output), OutputBellPairs output)
        (GASQ.GuardedAutomatonStepQuantum (test (STag output)) (PAOSQ.ProbAtomicOneStepPolicy output (STag output)))
        (CD p (OutputBellPairs output))
guardedAutomatonStage' ep initialState = Stage
    { stageName = "guarded_automaton'"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.execute ((. staticBellPairs) . getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith' ep') gfa
        initialState'
    }

guardedToSystemStage'
    :: forall p. RationalOrDouble p
    => forall output. (OpOutput output Op, OutputM output ~ CD')
    => (DDom (RTag output), Semigroup (CTag output))
    => forall test. (Test test, DecidableBoolean (test (STag output))) 
    => ExecutionParams (STag output) (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Stage (ExecutionParams (STag output) (RTag output) (CTag output), OutputBellPairs output)
        (GASQ.GuardedAutomatonStepQuantum (test (STag output)) (PAOSQ.ProbAtomicOneStepPolicy output (STag output)))
        (GASQ.StateSystem (CD p) (OutputBellPairs output))
guardedToSystemStage' ep initialState = Stage
    { stageName = "system_op_stage'"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.executeSystem ((. staticBellPairs) . getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith' ep') gfa
        initialState'
    }

guardedToSystemStage
    :: forall output. (OpOutput output Op, OutputM output ~ CD')
    => (DDom (RTag output), Semigroup (CTag output))
    => forall test. (Test test, DecidableBoolean (test (STag output))) 
    => ExecutionParams (STag output) (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Stage (ExecutionParams (STag output) (RTag output) (CTag output), OutputBellPairs output)
        (GASQ.GuardedAutomatonStepQuantum (test (STag output)) (PAOSQ.ProbAtomicOneStepPolicy output (STag output)))
        (GASQ.StateSystem CD' (OutputBellPairs output))
guardedToSystemStage ep initialState = Stage
    { stageName = "system_op_stage"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.executeSystem ((. staticBellPairs) . getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith' ep') gfa
        initialState'
    }

guardedToMDPStage'
    :: forall p. RationalOrDouble p
    => forall test. (Test test, DecidableBoolean (test DistillationCount), Show (test DistillationCount))
    => ExecutionParams DistillationCount QuantumTag MaxClock
    -> StaticBellPairs
    -> Stage (ExecutionParams DistillationCount QuantumTag MaxClock, StaticBellPairs)
        (GASQ.GuardedAutomatonStepQuantum (test DistillationCount) (PAOSQ.ProbAtomicOneStepPolicy (ListOutput QuantumOutput) DistillationCount))
        (GASQ.StateSystem (MDP p) StaticBellPairs)
guardedToMDPStage' ep initialState = Stage
    { stageName = "system_mdp_stage'"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        MDPP.minimizeStateSystem $
        GASQ.executeSystem MDPP.holdsStaticTest (MDPP.executeWith' ep') gfa
        initialState'
    }

guardedToWernerMDPStage'
    :: forall p rTag cTag test. RationalOrDouble p
    => (Test test, DecidableBoolean (test DistillationCount), Show (test DistillationCount))
    => ProbabilisticActionConfiguration
    -> ExecutionParams DistillationCount rTag cTag
    -> WernerBellPairs
    -> Stage (ExecutionParams DistillationCount rTag cTag, WernerBellPairs)
        (GASQ.GuardedAutomatonStepQuantum (test DistillationCount) (PAOSQ.ProbAtomicOneStepPolicy (ListOutput BinaryOutput) DistillationCount))
        (GASQ.StateSystem (MDP p) WernerBellPairs)
guardedToWernerMDPStage' pac ep initialState = Stage
    { stageName = "system_werner_mdp_stage'"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        MDPW.minimizeStateSystem $
        GASQ.executeSystem MDPW.holdsWernerGuardTest (MDPW.executeWith' pac ep') gfa
        initialState'
    }

-- | builds an automaton t`BellKAT.Utils.Automata.Guarded.GuardedFA` from a guarded policy `OrderedGuardedPolicy` using probabilistic interpretation configured via `ProbabilisticActionConfiguration` guided by `GASQ.GuardedAutomatonStepQuantum` with `PAOSQ.ProbAtomicOneStepPolicy` as an action.
probStarPolicyAutomatonPipeline
    :: forall output. (OpOutput output Op, Monoid output, Show output)
    => (Tag (STag output), Default (STag output))
    => forall test. (DecidableBoolean (test (STag output)), Show (test (STag output)))
    => Proxy output 
    -> ProbabilisticActionConfiguration
    -> Pipeline (Simple (OrderedGuardedPolicy (test (STag output))) (STag output))
        (GASQ.GuardedAutomatonStepQuantum (test (STag output)) (PAOSQ.ProbAtomicOneStepPolicy output (STag output)))
probStarPolicyAutomatonPipeline (_ :: Proxy output) pac
    = stage (probabilisticOpDesugarStage (Proxy :: Proxy (RTag output)) pac) 
    >>> stage probabilisticOpAutomatonStage

probStarPolicyOpPipeline'
    :: forall p. RationalOrDouble p
    => forall output. (OpOutput output Op, OutputM output ~ CD', Monoid output, Show output)
    => (Default (STag output), DDom (RTag output), Semigroup (CTag output))
    => forall test. (Test test, DecidableBoolean (test (STag output)), Show (test (STag output))) 
    => Proxy output
    -> ProbabilisticActionConfiguration
    -> ExecutionParams (STag output) (RTag output) (CTag output)
    -> OutputBellPairs output 
    -> Pipeline (Simple (OrderedGuardedPolicy (test (STag output))) (STag output)) (CD p (OutputBellPairs output))
probStarPolicyOpPipeline' proxy pac ep initialState = 
    probStarPolicyAutomatonPipeline proxy pac >>> stage (guardedAutomatonStage' ep initialState)

probStarPolicyOpPipeline
    :: forall output. (OpOutput output Op, Show (OutputM output (OutputBellPairs output)), Monoid output, Show output)
    => (Tag (STag output), Default (STag output), Tag (RTag output), Default (RTag output))
    => (Semigroup (CTag output), Tag (CTag output))
    => forall test. (Test test, DecidableBoolean (test (STag output)), Show (test (STag output))) 
    => Proxy output
    -> ProbabilisticActionConfiguration
    -> ExecutionParams (STag output) (RTag output) (CTag output)
    -> OutputBellPairs output 
    -> Pipeline (Simple (OrderedGuardedPolicy (test (STag output))) (STag output)) (OutputM output (OutputBellPairs output))
probStarPolicyOpPipeline proxy pac ep initialState = 
    probStarPolicyAutomatonPipeline proxy pac >>> stage (guardedAutomatonStage ep initialState)

probStarPolicyOpSystemPipeline'
    :: forall p. RationalOrDouble p
    => forall output. (OpOutput output Op, OutputM output ~ CD', Monoid output, Show output)
    => (Default (STag output), DDom (RTag output), Semigroup (CTag output))
    => forall test. (Test test, DecidableBoolean (test (STag output)), Show (test (STag output)))
    => Proxy output 
    -> ProbabilisticActionConfiguration 
    -> ExecutionParams (STag output) (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Pipeline (Simple (OrderedGuardedPolicy (test (STag output))) (STag output)) (GASQ.StateSystem (CD p) (OutputBellPairs output))
probStarPolicyOpSystemPipeline' proxy pac ep initialState = 
    probStarPolicyAutomatonPipeline proxy pac >>> stage (guardedToSystemStage' ep initialState)

probStarPolicyOpSystemPipeline
    :: forall output. (OpOutput output Op, OutputM output ~ CD', Monoid output, Show output)
    => (Default (STag output), DDom (RTag output)) 
    => Semigroup (CTag output)
    => forall test. (Test test, DecidableBoolean (test (STag output)), Show (test (STag output)))
    => Proxy output -> ProbabilisticActionConfiguration 
    -> ExecutionParams (STag output) (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Pipeline (Simple (OrderedGuardedPolicy (test (STag output))) (STag output)) (GASQ.StateSystem CD' (OutputBellPairs output))
probStarPolicyOpSystemPipeline proxy pac ep initialState = 
    probStarPolicyAutomatonPipeline proxy pac >>> stage (guardedToSystemStage ep initialState)

probStarPolicyQMDPPipeline'
    :: forall p. RationalOrDouble p
    => forall test. (Test test, DecidableBoolean (test DistillationCount), Show (test DistillationCount))
    => ProbabilisticActionConfiguration
    -> ExecutionParams DistillationCount QuantumTag MaxClock
    -> StaticBellPairs
    -> Pipeline (Simple (OrderedGuardedPolicy (test DistillationCount)) DistillationCount)
        (GASQ.StateSystem (MDP p) StaticBellPairs)
probStarPolicyQMDPPipeline' pac ep initialState =
    probStarPolicyAutomatonPipeline (Proxy :: Proxy (ListOutput QuantumOutput)) pac
        >>> stage (guardedToMDPStage' @p ep initialState)

probStarPolicyWMDPPipeline'
    :: forall p rTag cTag test. RationalOrDouble p
    => (Test test, DecidableBoolean (test DistillationCount), Show (test DistillationCount))
    => ProbabilisticActionConfiguration
    -> ExecutionParams DistillationCount rTag cTag
    -> WernerBellPairs
    -> Pipeline (Simple (OrderedGuardedPolicy (test DistillationCount)) DistillationCount)
        (GASQ.StateSystem (MDP p) WernerBellPairs)
probStarPolicyWMDPPipeline' pac ep initialState =
    probStarPolicyAutomatonPipeline (Proxy :: Proxy (ListOutput BinaryOutput)) pac
        >>> stage (guardedToWernerMDPStage' @p pac ep initialState)

applyProbStarPolicyOp'
    :: forall p. RationalOrDouble p
    => forall output. (OpOutput output Op, OutputM output ~ CD', Monoid output, Show output)
    => (Default (STag output), DDom (RTag output), Semigroup (CTag output))
    => forall test. (Test test, DecidableBoolean (test (STag output)), Show (test (STag output)))
    => Proxy output
    -> ProbabilisticActionConfiguration
    -> ExecutionParams (STag output) (RTag output) (CTag output)
    -> Simple (OrderedGuardedPolicy (test (STag output))) (STag output) 
    -> OutputBellPairs output 
    -> CD p (OutputBellPairs output)
applyProbStarPolicyOp' proxy pac ep policy initialState = 
    runNonLoggedPipeline (probStarPolicyOpPipeline' proxy pac ep initialState) policy
