{-# LANGUAGE TypeApplications #-}
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
import           BellKAT.Implementations.Output (RTag, CTag, OpOutput, staticBellPairs, Output (OutputM), OutputBellPairs)
import           BellKAT.Implementations.Configuration (ExecutionParams)
import           BellKAT.Bundles.Core

import qualified BellKAT.Implementations.GuardedAutomataStepQuantum    as GASQ
import qualified BellKAT.Implementations.ProbAtomicOneStepQuantum    as PAOSQ
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Identity

probabilisticOpDesugarStage
    :: (Functor f, Default rTag, CanDesugarActions (Op rTag) a)
    => Proxy rTag
    -> ProbabilisticActionConfiguration
    -> Stage ProbabilisticActionConfiguration (f a) (f (Desugared (Op rTag) a))
probabilisticOpDesugarStage (_ :: Proxy rTag) pac = Stage
    { stageName = "probabilistic_op_desugaring"
    , stageConfig = pac
    , stageFunction = mapDesugarActions @(Op rTag) . probabilisticOpActionMeaning
    }

probabilisticOpAutomatonStage
    :: (Default tag, DDom tag, Show (test tag), DecidableBoolean (test tag))
    => (OpOutput output (Op (RTag output)) tag, Monoid output, Ord output, Show output)
    => Stage ()
        (OrderedGuardedPolicy (test tag) (CreateBellPairArgs (Op (RTag output)) tag))
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy output tag))
probabilisticOpAutomatonStage = Stage
    { stageName = "constructing_guarded_automaton"
    , stageConfig = ()
    , stageFunction = \() -> meaning
    }

guardedAutomatonStage
    :: forall test tag output. (Ord tag, DecidableBoolean (test tag), Test test) 
    => (OpOutput output (Op (RTag output)) tag, Default (RTag output), DDom (RTag output)) 
    => (Semigroup (CTag output), Show (CTag output), Ord (CTag output), Typeable (CTag output))
    => ExecutionParams tag (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Stage (ExecutionParams tag (RTag output) (CTag output), OutputBellPairs output)
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy output tag))
        (OutputM output (OutputBellPairs output))
guardedAutomatonStage ep initialState = Stage
    { stageName = "executing_guarded_automaton"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.execute ((. staticBellPairs) . getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith ep') gfa
        initialState'
    }

guardedAutomatonStage'
    :: forall p test tag output. (Ord tag, DecidableBoolean (test tag), Test test, RationalOrDouble p) 
    => (OpOutput output (Op (RTag output)) tag, OutputM output ~ CD', Default (RTag output), DDom (RTag output)) 
    => (Semigroup (CTag output), Show (CTag output), Ord (CTag output), Typeable (CTag output))
    => ExecutionParams tag (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Stage (ExecutionParams tag (RTag output) (CTag output), OutputBellPairs output)
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy output tag))
        (CD p (OutputBellPairs output))
guardedAutomatonStage' ep initialState = Stage
    { stageName = "guarded_automaton'"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.execute ((. staticBellPairs) . getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith' ep') gfa
        initialState'
    }

guardedToSystemStage'
    :: forall p test tag output. (Ord tag, DecidableBoolean (test tag), Test test, RationalOrDouble p) 
    => (OpOutput output (Op (RTag output)) tag, OutputM output ~ CD', Default (RTag output), DDom (RTag output)) 
    => (Semigroup (CTag output), Show (CTag output), Ord (CTag output), Typeable (CTag output))
    => ExecutionParams tag (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Stage (ExecutionParams tag (RTag output) (CTag output), OutputBellPairs output)
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy output tag))
        (GASQ.StateSystem (CD p) (OutputBellPairs output))
guardedToSystemStage' ep initialState = Stage
    { stageName = "system_stage'"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.executeSystem ((. staticBellPairs) . getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith' ep') gfa
        initialState'
    }

guardedToSystemStage
    :: forall test tag output. (Ord tag, DecidableBoolean (test tag), Test test) 
    => (OpOutput output (Op (RTag output)) tag, OutputM output ~ CD', Default (RTag output), DDom (RTag output)) 
    => (Semigroup (CTag output), Show (CTag output), Ord (CTag output), Typeable (CTag output))
    => ExecutionParams tag (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Stage (ExecutionParams tag (RTag output) (CTag output), OutputBellPairs output)
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy output tag))
        (GASQ.StateSystem CD' (OutputBellPairs output))
guardedToSystemStage ep initialState = Stage
    { stageName = "system_stage"
    , stageConfig = (ep, initialState)
    , stageFunction = \(ep', initialState') gfa ->
        GASQ.executeSystem ((. staticBellPairs) . getBPsPredicate . toBPsPredicate) (PAOSQ.executeWith' ep') gfa
        initialState'
    }

-- | builds an automaton t`BellKAT.Utils.Automata.Guarded.GuardedFA` from a guarded policy `OrderedGuardedPolicy` using probabilistic interpretation configured via `ProbabilisticActionConfiguration` guided by `GASQ.GuardedAutomatonStepQuantum` with `PAOSQ.ProbAtomicOneStepPolicy` as an action.
probStarPolicyAutomatonPipeline
    :: (Default tag, DDom tag, Show (test tag), DecidableBoolean (test tag))
    => (OpOutput output (Op (RTag output)) tag, Default (RTag output), Monoid output, Ord output)
    => (Show output)
    => Proxy output 
    -> ProbabilisticActionConfiguration
    -> Pipeline (Simple (OrderedGuardedPolicy (test tag)) tag)
        (GASQ.GuardedAutomatonStepQuantum (test tag) (PAOSQ.ProbAtomicOneStepPolicy output tag))
probStarPolicyAutomatonPipeline (_ :: Proxy output) pac
    = stage (probabilisticOpDesugarStage (Proxy :: Proxy (RTag output)) pac) 
    >>> stage probabilisticOpAutomatonStage

probStarPolicyOpPipeline'
    :: forall p test tag output. (Typeable tag, Ord tag, Show tag, Default tag, DecidableBoolean (test tag), Test test, Show (test tag), Show p, RationalOrDouble p)
    => (OpOutput output (Op (RTag output)) tag, OutputM output ~ CD', Monoid output, Ord output, Show output, Default (RTag output), DDom (RTag output))
    => (Semigroup (CTag output), Show (CTag output), Ord (CTag output), Typeable (CTag output))
    => Proxy output
    -> ProbabilisticActionConfiguration
    -> ExecutionParams tag (RTag output) (CTag output)
    -> OutputBellPairs output 
    -> Pipeline (Simple (OrderedGuardedPolicy (test tag)) tag) (CD p (OutputBellPairs output))
probStarPolicyOpPipeline' proxy pac ep initialState = 
    probStarPolicyAutomatonPipeline proxy pac >>> stage (guardedAutomatonStage' ep initialState)

probStarPolicyOpPipeline
    :: forall test tag output. (Typeable tag, Ord tag, Show tag, Default tag, DecidableBoolean (test tag), Test test, Show (test tag))
    => (OpOutput output (Op (RTag output)) tag, Monoid output, Ord output, Show output, Default (RTag output), DDom (RTag output))
    => Show (OutputM output (OutputBellPairs output))
    => (Semigroup (CTag output), Show (CTag output), Ord (CTag output), Typeable (CTag output))
    => Proxy output
    -> ProbabilisticActionConfiguration
    -> ExecutionParams tag (RTag output) (CTag output)
    -> OutputBellPairs output 
    -> Pipeline (Simple (OrderedGuardedPolicy (test tag)) tag) (OutputM output (OutputBellPairs output))
probStarPolicyOpPipeline proxy pac ep initialState = 
    probStarPolicyAutomatonPipeline proxy pac >>> stage (guardedAutomatonStage ep initialState)

probStarPolicyOpSystemPipeline'
    :: forall p test tag output. 
        (RationalOrDouble p, Ord tag, Show tag, Typeable tag, Default tag, DecidableBoolean (test tag), Test test, Show (test tag))
    => (OpOutput output (Op (RTag output)) tag, OutputM output ~ CD', Monoid output, Ord output, Show output, Default (RTag output), DDom (RTag output)) 
    => (Semigroup (CTag output), Show (CTag output), Ord (CTag output), Typeable (CTag output))
    => Proxy output -> ProbabilisticActionConfiguration 
    -> ExecutionParams tag (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Pipeline (Simple (OrderedGuardedPolicy (test tag)) tag) (GASQ.StateSystem (CD p) (OutputBellPairs output))
probStarPolicyOpSystemPipeline' proxy pac ep initialState = 
    probStarPolicyAutomatonPipeline proxy pac >>> stage (guardedToSystemStage' ep initialState)

probStarPolicyOpSystemPipeline
    :: (Ord tag, Show tag, Typeable tag, Default tag, DecidableBoolean (test tag), Test test, Show (test tag))
    => (OpOutput output (Op (RTag output)) tag, OutputM output ~ CD', Monoid output, Ord output, Show output, Default (RTag output), DDom (RTag output)) 
    => (Semigroup (CTag output), Show (CTag output), Ord (CTag output), Typeable (CTag output))
    => Proxy output -> ProbabilisticActionConfiguration 
    -> ExecutionParams tag (RTag output) (CTag output)
    -> OutputBellPairs output
    -> Pipeline (Simple (OrderedGuardedPolicy (test tag)) tag) (GASQ.StateSystem CD' (OutputBellPairs output))
probStarPolicyOpSystemPipeline proxy pac ep initialState = 
    probStarPolicyAutomatonPipeline proxy pac >>> stage (guardedToSystemStage ep initialState)

applyProbStarPolicyOp'
    :: forall p test tag output. (Typeable tag, Ord tag, Show tag, Default tag, DecidableBoolean (test tag), Test test, Show (test tag), Show p, RationalOrDouble p)
    => (OpOutput output (Op (RTag output)) tag, OutputM output ~ CD', Monoid output, Ord output, Show output, Default (RTag output), DDom (RTag output))
    => (Semigroup (CTag output), Show (CTag output), Ord (CTag output), Typeable (CTag output))
    => Proxy output
    -> ProbabilisticActionConfiguration
    -> ExecutionParams tag (RTag output) (CTag output)
    -> Simple (OrderedGuardedPolicy (test tag)) tag 
    -> OutputBellPairs output 
    -> CD p (OutputBellPairs output)
applyProbStarPolicyOp' proxy pac ep policy initialState = 
    runIdentity . runNoLoggingT $ executePipeline (probStarPolicyOpPipeline' proxy pac ep initialState) policy

