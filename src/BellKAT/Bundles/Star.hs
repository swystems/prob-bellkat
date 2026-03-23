{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module BellKAT.Bundles.Star
    ( applyStarOrderedPolicy
    , applyStarPolicyWithValidity
    , applyStarOrderedPolicyBounded
    , applyStarPolicy
    , applyStarPolicy'
    , applyStarPolicyH
    , desugarStage
    , automatonStage
    , policyToAutomatonStage
    ) where

import           Data.Set                                (Set)
import           Data.Default

import           BellKAT.Definitions.Structures
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Tests
import           BellKAT.Definitions.Policy
import           BellKAT.Definitions.Policy.Extra
import           BellKAT.ActionEmbeddings
import           BellKAT.PolicyEmbeddings
import           BellKAT.Bundles.Core
import qualified BellKAT.Implementations.InterleavingOneStepHistoryQuantum as IOSHQ
import qualified BellKAT.Implementations.AutomataStepQuantum    as ASQ
import qualified BellKAT.Implementations.AtomicOneStepQuantum  as AOSQ

-- | A stage that desugars actions within a functorial structure using a simple interpretation.
--   It applies the 'simpleActionMeaning' to each action.
desugarStage
    :: (Functor f, CanDesugarActions' tag)
    => Stage () (f tag) (f (Desugared' tag))
desugarStage = Stage
    { stageName = "desugaring"
    , stageConfig = ()
    , stageFunction = \() -> mapDesugarActions simpleActionMeaning
    }

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

-- | A stage that builds an automaton from a policy.
policyToAutomatonStage
    :: (Ord tag, Default tag, Tests (AOSQ.AtomicOneStepPolicy tag) test tag)
    => Stage ()
        (Atomic CreateBellPairArgs' test tag)
        (ASQ.AutomatonStepQuantum 'ASQ.ACNormal (AOSQ.AtomicOneStepPolicy tag))
policyToAutomatonStage = Stage
    { stageName = "constructing_automaton"
    , stageConfig = ()
    , stageFunction = \() -> meaning
    }

-- * Policies with iteration, i.e., based on `OrderedStarPolicy`

-- | Main semantic function (e.g., used in the artifact)
applyStarPolicy
    :: (Ord tag, Show tag, Default tag, Tests (AOSQ.AtomicOneStepPolicy tag) test tag)
    => WithTests OrderedStarPolicy test tag -> TaggedBellPairs tag -> Set (TaggedBellPairs tag)
applyStarPolicy =
    ASQ.execute AOSQ.execute . meaning
        . mapDesugarActions simpleActionMeaning

applyStarPolicy'
    :: (Ord tag, Show tag, Default tag, Tests (AOSQ.AtomicOneStepPolicy tag) test tag)
    => ProbabilisticActionConfiguration
    -> WithTests OrderedStarPolicy test tag -> TaggedBellPairs tag -> Set (TaggedBellPairs tag)
applyStarPolicy' pac =
    ASQ.execute AOSQ.execute . meaning
        . mapDesugarActions (probabilisticActionMeaning pac)

applyStarOrderedPolicy
    :: (Ord tag, Show tag, Default tag, Test test)
    => WithTests OrderedStarPolicy test tag -> History tag -> Set (History tag)
applyStarOrderedPolicy =
    ASQ.executeE IOSHQ.execute . meaning . mapDesugarActions simpleActionMeaning
    . handleOrderingError . orderedAsSequence
  where
    handleOrderingError :: Maybe a -> a
    handleOrderingError Nothing = error "couldn't desugar ordered composition"
    handleOrderingError (Just x) = x

-- | executes `OrderedStarPolicy` with tests as function on histories
-- Warning: note, histories may grow arbitrary long with `OrderedStarPolicy`
applyStarPolicyH
    :: (Ord tag, Show tag, Default tag, Tests (IOSHQ.FunctionStep test tag) test tag)
    => WithTests OrderedStarPolicy test tag -> History tag -> Set (History tag)
applyStarPolicyH =
    ASQ.executeE IOSHQ.execute . meaning
    . mapDesugarActions simpleActionMeaning

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

-- | Similar to `applyStarPolicy` but errors if too many network state are observed in a given state
-- of policy execution
applyStarOrderedPolicyBounded
    :: (Ord tag, Show tag, Default tag, Test test)
    => WithTests OrderedStarPolicy test tag -> History tag -> Set (History tag)
applyStarOrderedPolicyBounded =
    (handleExecutionError .)
    . ASQ.executeWithE (def { ASQ.maxOptionsPerState = Just 100}) IOSHQ.execute
    . meaning . mapDesugarActions simpleActionMeaning . handleOrderingError . orderedAsSequence
  where
    handleExecutionError :: Maybe a -> a
    handleExecutionError Nothing = error "couldn't execute"
    handleExecutionError (Just x) = x

    handleOrderingError :: Maybe a -> a
    handleOrderingError Nothing = error "couldn't desugar ordered composition"
    handleOrderingError (Just x) = x
