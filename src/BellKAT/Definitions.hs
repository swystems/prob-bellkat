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

applyPolicy :: Ord tag => Normal Policy tag -> History tag -> Set (History tag)
applyPolicy = HQ.execute . meaning . desugarActions simpleActionMeaning

applyPolicyTimely :: Ord tag => Normal Policy tag -> History tag -> Set (History tag)
applyPolicyTimely = THQ.execute . meaning . desugarActions simpleActionMeaning

applyPolicySteps :: (Ord tag) => Normal Policy tag -> History tag -> Set (History tag)
applyPolicySteps  = SHQ.execute HQ.execute . meaning . desugarActions simpleActionMeaning

applyOrderedPolicy 
    :: (Ord tag, Show tag) 
    => Ordered Policy BellPairsPredicate tag -> History tag -> Set (History tag)
applyOrderedPolicy = 
    SHQ.execute IOSHQ.execute . meaning . desugarActions simpleActionMeaning

applyFullOrderedPolicy 
    :: (Ord tag, Show tag) 
    => Ordered FullPolicy BellPairsPredicate tag -> History tag -> Set (History tag)
applyFullOrderedPolicy = SHQ.execute IOSHQ.execute . meaning . desugarActions simpleActionMeaning

applyFullOrderedPolicyAuto 
    :: (Ord tag, Show tag, Default tag) 
    => Ordered FullPolicy BellPairsPredicate tag -> History tag -> Set (History tag)
applyFullOrderedPolicyAuto = 
    ASHQ.executeE IOSHQ.execute . meaning . desugarActions simpleActionMeaning

applyStarOrderedPolicy 
    :: (Ord tag, Show tag, Default tag) 
    => Ordered StarPolicy BellPairsPredicate tag -> History tag -> Set (History tag)
applyStarOrderedPolicy = ASHQ.executeE IOSHQ.execute . meaning . desugarActions simpleActionMeaning

applyStarPolicy 
    :: (Ord tag, Show tag, Default tag, Tests (AOSHQ.AtomicOneStepPolicy tag) test tag) 
    => NormalWithTests StarPolicy test tag -> TaggedBellPairs tag -> Set (TaggedBellPairs tag)
applyStarPolicy = 
    ASHQ.execute AOSHQ.execute . meaning 
        . desugarActions simpleActionMeaning . setDupKinds (DupKind True False)

applyStarPolicyH
    :: (Ord tag, Show tag, Default tag, Tests (IOSHQ.FunctionStep test tag) test tag) 
    => NormalWithTests StarPolicy test tag -> History tag -> Set (History tag)
applyStarPolicyH = 
    ASHQ.executeE IOSHQ.execute . meaning 
    . desugarActions simpleActionMeaning . setDupKinds (DupKind True False)

applyStarPolicyWithValidity
    :: (Ord tag, Show tag, Default tag, Tests (AOSHQ.AtomicOneStepPolicy tag) test tag) 
    => (TaggedBellPairs tag -> Bool)
    -> NormalWithTests StarPolicy test tag 
    -> TaggedBellPairs tag 
    -> Maybe (Set (TaggedBellPairs tag))
applyStarPolicyWithValidity isValid = 
    ASHQ.executeWith (def { ASHQ.isValidState = isValid }) AOSHQ.execute 
    . meaning  . desugarActions simpleActionMeaning

applyOneStepPolicyPartial 
    :: (Ord tag, Show tag) 
    => Normal OneRoundPolicy tag -> History tag -> Set (Partial (History tag))
applyOneStepPolicyPartial = IOSHQ.executePartial . meaning . desugarActions simpleActionMeaning

applyOneStepPolicy 
    :: (Ord tag, Show tag) 
    => Normal OneRoundPolicy tag -> History tag -> Set (History tag)
applyOneStepPolicy = IOSHQ.execute . meaning . desugarActions simpleActionMeaning

applyStarOrderedPolicyBounded 
    :: (Ord tag, Show tag, Default tag) 
    => Ordered StarPolicy BellPairsPredicate tag -> History tag -> Set (History tag)
applyStarOrderedPolicyBounded = 
    (handleExecutionError .) 
    . ASHQ.executeWithE (def { ASHQ.maxOptionsPerState = Just 100}) IOSHQ.execute
    . meaning . desugarActions simpleActionMeaning
  where
    handleExecutionError :: Maybe a -> a
    handleExecutionError Nothing = error "couldn't execute"
    handleExecutionError (Just x) = x
