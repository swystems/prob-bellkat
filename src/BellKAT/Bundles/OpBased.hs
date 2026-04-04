{-# LANGUAGE OverloadedStrings #-}
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
import           BellKAT.Bundles.Core
import           BellKAT.Bundles.Desugaring (probabilisticOpDesugarStage)

import qualified BellKAT.Implementations.GuardedAutomataStepQuantum    as GASQ
import qualified BellKAT.Implementations.ProbAtomicOneStepQuantum    as PAOSQ

probabilisticOpAutomatonStage
    :: (OpOutput output (Op (RTag output)), Monoid output, Show output)
    => Default (STag output)
    => (DecidableBoolean (test (STag output)), Show (test (STag output)))
    => Stage ()
        (OrderedGuardedPolicy (test (STag output)) (CreateBellPairArgs (Op (RTag output)) (STag output)))
        (GASQ.GuardedAutomatonStepQuantum (test (STag output)) (PAOSQ.ProbAtomicOneStepPolicy output (STag output)))
probabilisticOpAutomatonStage = Stage
    { stageName = "constructing_guarded_op_automaton"
    , stageConfig = ()
    , stageFunction = \() -> meaning
    }

guardedAutomatonStage
    :: OpOutput output (Op (RTag output))
    => Default (RTag output) 
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
        GASQ.execute ((. staticBellPairs) . testBellPairs) (PAOSQ.executeWith ep') gfa
        initialState'
    }

guardedAutomatonStage'
    :: forall p. RationalOrDouble p
    => forall output. (OpOutput output (Op (RTag output)), OutputM output ~ CD')
    => (Default (RTag output), DDom (RTag output), Semigroup (CTag output))
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
        GASQ.execute ((. staticBellPairs) . testBellPairs) (PAOSQ.executeWith' ep') gfa
        initialState'
    }

guardedToSystemStage'
    :: forall p. RationalOrDouble p
    => forall output. (OpOutput output (Op (RTag output)), OutputM output ~ CD')
    => (DDom (RTag output), Default (RTag output), Semigroup (CTag output))
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
        GASQ.executeSystem ((. staticBellPairs) . testBellPairs) (PAOSQ.executeWith' ep') gfa
        initialState'
    }

guardedToSystemStage
    :: forall output. (OpOutput output (Op (RTag output)), OutputM output ~ CD')
    => (DDom (RTag output), Default (RTag output), Semigroup (CTag output))
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
        GASQ.executeSystem ((. staticBellPairs) . testBellPairs) (PAOSQ.executeWith' ep') gfa
        initialState'
    }

-- | builds an automaton t`BellKAT.Utils.Automata.Guarded.GuardedFA` from a guarded policy `OrderedGuardedPolicy` using probabilistic interpretation configured via `ProbabilisticActionConfiguration` guided by `GASQ.GuardedAutomatonStepQuantum` with `PAOSQ.ProbAtomicOneStepPolicy` as an action.
probStarPolicyAutomatonPipeline
    :: forall output. (OpOutput output (Op (RTag output)), Monoid output, Show output)
    => (Tag (STag output), Default (STag output), Default (RTag output))
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
    => forall output. (OpOutput output (Op (RTag output)), OutputM output ~ CD', Monoid output, Show output)
    => (Default (STag output), DDom (RTag output), Default (RTag output), Semigroup (CTag output))
    => forall test. (Test test, DecidableBoolean (test (STag output)), Show (test (STag output))) 
    => Proxy output
    -> ProbabilisticActionConfiguration
    -> ExecutionParams (STag output) (RTag output) (CTag output)
    -> OutputBellPairs output 
    -> Pipeline (Simple (OrderedGuardedPolicy (test (STag output))) (STag output)) (CD p (OutputBellPairs output))
probStarPolicyOpPipeline' proxy pac ep initialState = 
    probStarPolicyAutomatonPipeline proxy pac >>> stage (guardedAutomatonStage' ep initialState)

probStarPolicyOpPipeline
    :: forall output. (OpOutput output (Op (RTag output)), Show (OutputM output (OutputBellPairs output)), Monoid output, Show output)
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
    => forall output. (OpOutput output (Op (RTag output)), OutputM output ~ CD', Monoid output, Show output)
    => (Default (STag output), DDom (RTag output), Default (RTag output), Semigroup (CTag output))
    => forall test. (Test test, DecidableBoolean (test (STag output)), Show (test (STag output)))
    => Proxy output 
    -> ProbabilisticActionConfiguration 
    -> ExecutionParams (STag output) (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Pipeline (Simple (OrderedGuardedPolicy (test (STag output))) (STag output)) (GASQ.StateSystem (CD p) (OutputBellPairs output))
probStarPolicyOpSystemPipeline' proxy pac ep initialState = 
    probStarPolicyAutomatonPipeline proxy pac >>> stage (guardedToSystemStage' ep initialState)

probStarPolicyOpSystemPipeline
    :: forall output. (OpOutput output (Op (RTag output)), OutputM output ~ CD', Monoid output, Show output)
    => (Default (STag output), DDom (RTag output), Default (RTag output)) 
    => Semigroup (CTag output)
    => forall test. (Test test, DecidableBoolean (test (STag output)), Show (test (STag output)))
    => Proxy output -> ProbabilisticActionConfiguration 
    -> ExecutionParams (STag output) (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Pipeline (Simple (OrderedGuardedPolicy (test (STag output))) (STag output)) (GASQ.StateSystem CD' (OutputBellPairs output))
probStarPolicyOpSystemPipeline proxy pac ep initialState = 
    probStarPolicyAutomatonPipeline proxy pac >>> stage (guardedToSystemStage ep initialState)

applyProbStarPolicyOp'
    :: forall p. RationalOrDouble p
    => forall output. (OpOutput output (Op (RTag output)), OutputM output ~ CD', Monoid output, Show output)
    => (Default (STag output), Default (RTag output), DDom (RTag output), Semigroup (CTag output))
    => forall test. (Test test, DecidableBoolean (test (STag output)), Show (test (STag output)))
    => Proxy output
    -> ProbabilisticActionConfiguration
    -> ExecutionParams (STag output) (RTag output) (CTag output)
    -> Simple (OrderedGuardedPolicy (test (STag output))) (STag output) 
    -> OutputBellPairs output 
    -> CD p (OutputBellPairs output)
applyProbStarPolicyOp' proxy pac ep policy initialState = 
    runNonLoggedPipeline (probStarPolicyOpPipeline' proxy pac ep initialState) policy

