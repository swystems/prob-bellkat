{-# LANGUAGE DataKinds #-}
module BellKAT.Definitions
    ( module BellKAT.Definitions.Core
    , module BellKAT.Definitions.Tests
    , module BellKAT.Definitions.Structures
    , module BellKAT.Definitions.Policy
    , module BellKAT.Bundles.Star
    , applyPolicy
    , applyPolicyTimely
    , applyPolicySteps
    , applyOrderedPolicy
    , applyFullOrderedPolicy
    , applyFullOrderedPolicyAuto
    , applyOneStepPolicy
    , applyOneStepPolicyPartial
    ) where

import           Data.Set                                (Set)
import           Data.Default

import           BellKAT.Definitions.Structures
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Tests
import           BellKAT.Definitions.Policy
import           BellKAT.ActionEmbeddings 
import           BellKAT.PolicyEmbeddings
import qualified BellKAT.Implementations.HistoryQuantum        as HQ
import qualified BellKAT.Implementations.InterleavingOneStepHistoryQuantum as IOSHQ
import qualified BellKAT.Implementations.StepHistoryQuantum    as SHQ
import qualified BellKAT.Implementations.AutomataStepQuantum    as ASQ
import qualified BellKAT.Implementations.TimelyHistoryQuantum  as THQ
import           BellKAT.Bundles.Star

-- | = Deterministic (no explicit choice operator) one round policies
applyPolicy :: Ord tag => Simple Policy tag -> History tag -> Set (History tag)
applyPolicy = HQ.execute . meaning . mapDesugarActions simpleActionMeaning

applyPolicyTimely :: Ord tag => Simple Policy tag -> History tag -> Set (History tag)
applyPolicyTimely = THQ.execute . meaning . mapDesugarActions simpleActionMeaning

applyPolicySteps :: (Ord tag) => Simple Policy tag -> History tag -> Set (History tag)
applyPolicySteps  = SHQ.execute HQ.execute . meaning . mapDesugarActions simpleActionMeaning

-- * Nondeterministic one round policies

applyOneStepPolicyPartial 
    :: (Ord tag, Show tag) 
    => Simple OneRoundPolicy tag -> History tag -> Set (Partial (History tag))
applyOneStepPolicyPartial = IOSHQ.executePartial . meaning . mapDesugarActions simpleActionMeaning

applyOneStepPolicy 
    :: (Ord tag, Show tag) 
    => Simple OneRoundPolicy tag -> History tag -> Set (History tag)
applyOneStepPolicy = IOSHQ.execute . meaning . mapDesugarActions simpleActionMeaning

-- * Policies with fake ordered composition (one that just collects component in a sequence)

applyOrderedPolicy 
    :: (Ord tag, Show tag, Test test) 
    => SeqWithTests Policy test tag -> History tag -> Set (History tag)
applyOrderedPolicy = 
    SHQ.execute IOSHQ.execute . meaning . mapDesugarActions simpleActionMeaning

applyFullOrderedPolicy 
    :: (Ord tag, Show tag, Test test) 
    => SeqWithTests FullPolicy test tag -> History tag -> Set (History tag)
applyFullOrderedPolicy = SHQ.execute IOSHQ.execute . meaning . mapDesugarActions simpleActionMeaning

applyFullOrderedPolicyAuto 
    :: (Ord tag, Show tag, Default tag, Test test) 
    => SeqWithTests FullPolicy test tag -> History tag -> Set (History tag)
applyFullOrderedPolicyAuto = 
    ASQ.executeE IOSHQ.execute . meaning . mapDesugarActions simpleActionMeaning

