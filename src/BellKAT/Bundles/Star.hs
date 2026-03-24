{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module BellKAT.Bundles.Star
    ( applyStarPolicyWithValidity
    , applyStarPolicy
    , applyStarPolicy'
    ) where

import           Data.Set                                (Set)
import           Data.Default

import           BellKAT.Definitions.Structures
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Policy
import           BellKAT.ActionEmbeddings
import           BellKAT.PolicyEmbeddings
import           BellKAT.Bundles.Core
import           BellKAT.Bundles.Desugaring
import qualified BellKAT.Implementations.AutomataStepQuantum    as ASQ
import qualified BellKAT.Implementations.AtomicOneStepQuantum  as AOSQ

-- | A stage that executes a policy automaton on a set of Bell pairs.
automatonStage
    :: (Ord tag, Show tag, Default tag)
    => TaggedBellPairs tag
    -> Stage
        (TaggedBellPairs tag)
        (ASQ.AutomatonStepQuantum 'ASQ.ACNormal (AOSQ.AtomicOneStepPolicy tag))
        (Set (TaggedBellPairs tag))
automatonStage initialState = Stage
    { stageName = "executing_automaton"
    , stageConfig = initialState
    , stageFunction = flip (ASQ.execute AOSQ.execute)
    }

-- * Policies with iteration, i.e., based on `OrderedStarPolicy`

-- | A stage that builds an automaton from an ordered star policy.
policyToAutomatonStage
    :: (Ord tag, Default tag, Tests (AOSQ.AtomicOneStepPolicy tag) test tag)
    => Stage ()
        (OrderedStarPolicy (Atomic CreateBellPairArgs' test tag))
        (ASQ.AutomatonStepQuantum 'ASQ.ACNormal (AOSQ.AtomicOneStepPolicy tag))
policyToAutomatonStage = Stage
    { stageName = "constructing_automaton"
    , stageConfig = ()
    , stageFunction = \() -> meaning
    }

starPolicyPipeline
    :: (Ord tag, Show tag, Show (test tag), Default tag, Tests (AOSQ.AtomicOneStepPolicy tag) test tag)
    => TaggedBellPairs tag
    -> Pipeline (WithTests OrderedStarPolicy test tag) (Set (TaggedBellPairs tag))
starPolicyPipeline initialState =
    stage desugarStage >>> stage policyToAutomatonStage >>> stage (automatonStage initialState)

starPolicyPipeline'
    :: (Ord tag, Show tag, Show (test tag), Default tag, Tests (AOSQ.AtomicOneStepPolicy tag) test tag)
    => ProbabilisticActionConfiguration
    -> TaggedBellPairs tag
    -> Pipeline (WithTests OrderedStarPolicy test tag) (Set (TaggedBellPairs tag))
starPolicyPipeline' pac initialState =
    stage (probabilisticDesugarStage pac) >>> stage policyToAutomatonStage >>> stage (automatonStage initialState)

-- | Main semantic function (e.g., used in the artifact)
applyStarPolicy
    :: (Ord tag, Show tag, Show (test tag), Default tag, Tests (AOSQ.AtomicOneStepPolicy tag) test tag)
    => WithTests OrderedStarPolicy test tag -> TaggedBellPairs tag -> Set (TaggedBellPairs tag)
applyStarPolicy policy initialState =
    runNonLoggedPipeline (starPolicyPipeline initialState) policy

applyStarPolicy'
    :: (Ord tag, Show tag, Show (test tag), Default tag, Tests (AOSQ.AtomicOneStepPolicy tag) test tag)
    => ProbabilisticActionConfiguration
    -> WithTests OrderedStarPolicy test tag -> TaggedBellPairs tag -> Set (TaggedBellPairs tag)
applyStarPolicy' pac policy initialState =
    runNonLoggedPipeline (starPolicyPipeline' pac initialState) policy

-- | Similar to `applyStarPolicy` but returns `Nothing` if an invalid state is ever reached
applyStarPolicyWithValidity
    :: (Ord tag, Show tag, Default tag, Tests (AOSQ.AtomicOneStepPolicy tag) test tag)
    => (TaggedBellPairs tag -> Bool)
    -> WithTests OrderedStarPolicy test tag
    -> TaggedBellPairs tag
    -> Maybe (Set (TaggedBellPairs tag))
applyStarPolicyWithValidity isValid =
    ASQ.executeWith (def { ASQ.isValidState = isValid }) AOSQ.execute
    . meaning  . mapDesugarActions simpleActionMeaning
