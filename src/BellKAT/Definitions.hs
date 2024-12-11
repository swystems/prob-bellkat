{-# LANGUAGE DataKinds #-}
module BellKAT.Definitions
    ( module BellKAT.Definitions.Core
    , module BellKAT.Definitions.Structures
    , module BellKAT.Definitions.Policy
    , applyPolicy
    , applyPolicyTimely
    , applyPolicySteps
    , applyOrderedPolicy
    , applyFullOrderedPolicy
    , applyFullOrderedPolicyAuto
    , applyStarOrderedPolicy
    , applyStarPolicyWithValidity
    , applyStarOrderedPolicyBounded
    , applyOneStepPolicy
    , applyOneStepPolicyPartial
    , applyStarPolicy
    , applyStarPolicyH
    ) where

import           Data.Set                                (Set)
import           Data.Default

import           BellKAT.Definitions.Structures
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Policy
import           BellKAT.ActionEmbeddings 
import           BellKAT.PolicyEmbeddings
import qualified BellKAT.Implementations.HistoryQuantum        as HQ
import qualified BellKAT.Implementations.InterleavingOneStepHistoryQuantum as IOSHQ
import qualified BellKAT.Implementations.StepHistoryQuantum    as SHQ
import qualified BellKAT.Implementations.AutomataStepHistoryQuantum    as ASHQ
import qualified BellKAT.Implementations.AtomicOneStepHistoryQuantum  as AOSHQ
import qualified BellKAT.Implementations.TimelyHistoryQuantum  as THQ

-- * Deterministic (no explicit choice operator) one round policies

applyPolicy :: Ord tag => Simple Policy tag -> History tag -> Set (History tag)
applyPolicy = HQ.execute . meaning . desugarActions simpleActionMeaning

applyPolicyTimely :: Ord tag => Simple Policy tag -> History tag -> Set (History tag)
applyPolicyTimely = THQ.execute . meaning . desugarActions simpleActionMeaning

applyPolicySteps :: (Ord tag) => Simple Policy tag -> History tag -> Set (History tag)
applyPolicySteps  = SHQ.execute HQ.execute . meaning . desugarActions simpleActionMeaning

-- * Nondeterministic one round policies

applyOneStepPolicyPartial 
    :: (Ord tag, Show tag) 
    => Simple OneRoundPolicy tag -> History tag -> Set (Partial (History tag))
applyOneStepPolicyPartial = IOSHQ.executePartial . meaning . desugarActions simpleActionMeaning

applyOneStepPolicy 
    :: (Ord tag, Show tag) 
    => Simple OneRoundPolicy tag -> History tag -> Set (History tag)
applyOneStepPolicy = IOSHQ.execute . meaning . desugarActions simpleActionMeaning

-- * Policies with fake ordered composition (one that just collects component in a sequence)

applyOrderedPolicy 
    :: (Ord tag, Show tag, Test test) 
    => SeqWithTests Policy test tag -> History tag -> Set (History tag)
applyOrderedPolicy = 
    SHQ.execute IOSHQ.execute . meaning . desugarActions simpleActionMeaning

applyFullOrderedPolicy 
    :: (Ord tag, Show tag, Test test) 
    => SeqWithTests FullPolicy test tag -> History tag -> Set (History tag)
applyFullOrderedPolicy = SHQ.execute IOSHQ.execute . meaning . desugarActions simpleActionMeaning

applyFullOrderedPolicyAuto 
    :: (Ord tag, Show tag, Default tag, Test test) 
    => SeqWithTests FullPolicy test tag -> History tag -> Set (History tag)
applyFullOrderedPolicyAuto = 
    ASHQ.executeE IOSHQ.execute . meaning . desugarActions simpleActionMeaning

-- * Policies with iteration, i.e., based on `OrderedStarPolicy`

-- | Main semantic function (e.g., used in the artifact)
applyStarPolicy 
    :: (Ord tag, Show tag, Default tag, Tests (AOSHQ.AtomicOneStepPolicy tag) test tag) 
    => WithTests OrderedStarPolicy test tag -> TaggedBellPairs tag -> Set (TaggedBellPairs tag)
applyStarPolicy = 
    ASHQ.execute AOSHQ.execute . meaning 
        . desugarActions simpleActionMeaning . setDupKinds (DupKind True False)

applyStarOrderedPolicy
    :: (Ord tag, Show tag, Default tag, Test test) 
    => SeqWithTests OrderedStarPolicy test tag -> History tag -> Set (History tag)
applyStarOrderedPolicy = ASHQ.executeE IOSHQ.execute . meaning . desugarActions simpleActionMeaning

-- | executes `OrderedStarPolicy` with tests as function on histories 
-- Warning: note, histories may grow arbitrary long with `OrderedStarPolicy`
applyStarPolicyH
    :: (Ord tag, Show tag, Default tag, Tests (IOSHQ.FunctionStep test tag) test tag) 
    => WithTests OrderedStarPolicy test tag -> History tag -> Set (History tag)
applyStarPolicyH = 
    ASHQ.executeE IOSHQ.execute . meaning 
    . desugarActions simpleActionMeaning . setDupKinds (DupKind True False)

-- | Similar to `applyStarPolicy` but returns `Nothing` if an invalid state is ever reached
applyStarPolicyWithValidity
    :: (Ord tag, Show tag, Default tag, Tests (AOSHQ.AtomicOneStepPolicy tag) test tag) 
    => (TaggedBellPairs tag -> Bool)
    -> WithTests OrderedStarPolicy test tag 
    -> TaggedBellPairs tag 
    -> Maybe (Set (TaggedBellPairs tag))
applyStarPolicyWithValidity isValid = 
    ASHQ.executeWith (def { ASHQ.isValidState = isValid }) AOSHQ.execute 
    . meaning  . desugarActions simpleActionMeaning

-- | Similar to `applyStarPolicy` but errors if too many network state are observed in a given state
-- of policy execution
applyStarOrderedPolicyBounded
    :: (Ord tag, Show tag, Default tag, Test test) 
    => SeqWithTests OrderedStarPolicy test tag -> History tag -> Set (History tag)
applyStarOrderedPolicyBounded = 
    (handleExecutionError .) 
    . ASHQ.executeWithE (def { ASHQ.maxOptionsPerState = Just 100}) IOSHQ.execute
    . meaning . desugarActions simpleActionMeaning
  where
    handleExecutionError :: Maybe a -> a
    handleExecutionError Nothing = error "couldn't execute"
    handleExecutionError (Just x) = x
