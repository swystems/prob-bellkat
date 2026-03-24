module BellKAT.Bundles.HistoryBased
    ( applyPolicy
    , applyPolicyTimely
    , applyPolicySteps
    , applyOrderedPolicy
    , applyFullOrderedPolicy
    , applyFullOrderedPolicyAuto
    , applyOneStepPolicy
    , applyOneStepPolicyPartial
    , applyStarOrderedPolicyBounded
    , applyStarOrderedPolicy
    , applyStarPolicyH
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
import qualified BellKAT.Implementations.HistoryQuantum        as HQ
import qualified BellKAT.Implementations.InterleavingOneStepHistoryQuantum as IOSHQ
import qualified BellKAT.Implementations.StepHistoryQuantum    as SHQ
import qualified BellKAT.Implementations.AutomataStepQuantum    as ASQ
import qualified BellKAT.Implementations.TimelyHistoryQuantum  as THQ

-- | = Deterministic (no explicit choice operator) one round policies
applyPolicy :: Tag tag => Simple Policy tag -> History tag -> Set (History tag)
applyPolicy = HQ.execute . meaning . mapDesugarActions simpleActionMeaning

applyPolicyTimely :: Tag tag => Simple Policy tag -> History tag -> Set (History tag)
applyPolicyTimely = THQ.execute . meaning . mapDesugarActions simpleActionMeaning

applyPolicySteps :: Tag tag => Simple Policy tag -> History tag -> Set (History tag)
applyPolicySteps  = SHQ.execute HQ.execute . meaning . mapDesugarActions simpleActionMeaning

-- * Nondeterministic one round policies

applyOneStepPolicyPartial 
    :: Tag tag
    => Simple OneRoundPolicy tag -> History tag -> Set (Partial (History tag))
applyOneStepPolicyPartial = IOSHQ.executePartial . meaning . mapDesugarActions simpleActionMeaning

applyOneStepPolicy 
    :: Tag tag
    => Simple OneRoundPolicy tag -> History tag -> Set (History tag)
applyOneStepPolicy = IOSHQ.execute . meaning . mapDesugarActions simpleActionMeaning

-- * Policies with fake ordered composition (one that just collects component in a sequence)

applyOrderedPolicy 
    :: (Tag tag, Test test) 
    => SeqWithTests Policy test tag -> History tag -> Set (History tag)
applyOrderedPolicy = 
    SHQ.execute IOSHQ.execute . meaning . mapDesugarActions simpleActionMeaning

applyFullOrderedPolicy 
    :: (Tag tag, Test test) 
    => SeqWithTests FullPolicy test tag -> History tag -> Set (History tag)
applyFullOrderedPolicy = SHQ.execute IOSHQ.execute . meaning . mapDesugarActions simpleActionMeaning

applyFullOrderedPolicyAuto 
    :: (Tag tag, Default tag, Test test) 
    => SeqWithTests FullPolicy test tag -> History tag -> Set (History tag)
applyFullOrderedPolicyAuto = 
    ASQ.executeE IOSHQ.execute . meaning . mapDesugarActions simpleActionMeaning

applyStarOrderedPolicy
    :: (Tag tag, Default tag, Test test)
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
    :: (Tag tag, Default tag, Tests (IOSHQ.FunctionStep test tag) test tag)
    => WithTests OrderedStarPolicy test tag -> History tag -> Set (History tag)
applyStarPolicyH =
    ASQ.executeE IOSHQ.execute . meaning
    . mapDesugarActions simpleActionMeaning

-- | Similar to `applyStarPolicy` but errors if too many network state are observed in a given state
-- of policy execution
applyStarOrderedPolicyBounded
    :: (Tag tag, Default tag, Test test)
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
