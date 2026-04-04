{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module BellKAT.ProbabilisticPrelude (
    -- * PBKAT Policy syntax
    BellKATTagChar(..),
    BellKATTag,
    ProbBellKATTest,
    ProbBellKATAction,
    ProbBellKATPolicy,
    -- * Re-exports network configuration
    ProbabilisticActionConfiguration(..),
    NetworkCapacity,
    -- * Entry points
    pbkatMain,
    pbkatMainD,
    pbkatMainWithOpts,
    pbkatMainWithOptsD,
    -- * Auxiliary expression generation exports
    stimes,
    -- * Re-exports from 'BellKAT.DSL'
    module BellKAT.DSL,
    (~),
    -- * Re-exports from 'BellKAT.Definitions.Structures'
    module BellKAT.Definitions.Structures,
    -- * Re-exports from 'BellKAT.ActionEmbeddings'
) where

import Data.Typeable
import qualified Data.Aeson as A
import Data.Semigroup (stimes)

import BellKAT.Prelude
import BellKAT.DSL
import BellKAT.Definitions
import BellKAT.Definitions.Structures
import BellKAT.ActionEmbeddings (ProbabilisticActionConfiguration(..))
import BellKAT.Implementations.Configuration (NetworkCapacity, fromNetworkCapacity)
import BellKAT.Utils.Distribution (RationalOrDouble)
import BellKAT.Prelude.Common
import BellKAT.Bundles.Probabilistic
import BellKAT.Utils.Automata.Transitions.Functorial
import BellKAT.Utils.Convex
import qualified BellKAT.Implementations.GuardedAutomataStepQuantum as GASQ
import qualified BellKAT.Implementations.ProbAtomicOneStepQuantum as PAOSQ

type ProbBellKATTest = BoundedTest BellKATTag
type ProbBellKATAction = TaggedAction BellKATTag

type ProbBellKATPolicy = OrderedGuardedPolicy ProbBellKATTest ProbBellKATAction

type PBKATSystem p = StateSystem (CD p) (TaggedBellPairs BellKATTag)
type PBKATAutomaton = GASQ.GuardedAutomatonStepQuantum ProbBellKATTest (PAOSQ.ProbAtomicOneStepPolicy' BellKATTag)

-- | Prepares 'RunPipelines' for PBKAT.
createProbabilisticPipelines
    :: forall p. (RationalOrDouble p, A.ToJSON p, A.FromJSON p)
    => Proxy p
    -> ProbabilisticActionConfiguration
    -> Maybe (NetworkCapacity BellKATTag)
    -> RunPipelines p ProbBellKATPolicy 
        (PBKATSystem p) PBKATAutomaton (TaggedBellPairs BellKATTag)
createProbabilisticPipelines _ pac mbNC =
    let ep = fromNetworkCapacity mbNC
    in RunPipelines
        { runPipeline = probStarPolicyPipeline' @p pac ep mempty
        , systemPipeline = probStarPolicySystemPipeline' @p pac ep mempty
        , automatonPipeline = probStarPolicyAutomatonPipeline pac
        }

pbkatMain'
    :: (RationalOrDouble p, A.ToJSON p, A.FromJSON p)
    => Proxy p
    -> ProbabilisticActionConfiguration
    -> Maybe (NetworkCapacity BellKATTag)
    -> ProbBellKATTest
    -> ProbBellKATPolicy
    -> IO ()
pbkatMain' p pac mbNC ev protocol =
    let rp = createProbabilisticPipelines p pac mbNC
    in main rp "PBKAT tool" protocol (testBellPairs ev)

-- | speicialization of `pbkatMain'` to rational probability `Probability`
pbkatMain
    :: ProbabilisticActionConfiguration
    -> Maybe (NetworkCapacity BellKATTag)
    -> ProbBellKATTest
    -> ProbBellKATPolicy
    -> IO ()
pbkatMain = pbkatMain' (Proxy :: Proxy Probability)

-- | speicialization of `pbkatMain'` to floating point probability `Double`
pbkatMainD
    :: ProbabilisticActionConfiguration
    -> Maybe (NetworkCapacity BellKATTag)
    -> ProbBellKATTest
    -> ProbBellKATPolicy
    -> IO ()
pbkatMainD = pbkatMain' (Proxy :: Proxy Double)

pbkatMainWithOpts'
    :: forall p. (RationalOrDouble p, A.ToJSON p, A.FromJSON p)
    => Proxy p
    -> KatCLIOpts
    -> ProbabilisticActionConfiguration
    -> Maybe (NetworkCapacity BellKATTag)
    -> ProbBellKATTest
    -> ProbBellKATPolicy
    -> IO ()
pbkatMainWithOpts' p opts pac mbNC ev protocol =
    let rp = createProbabilisticPipelines p pac mbNC
    in mainWithOpts rp opts protocol (testBellPairs ev)

pbkatMainWithOpts
    :: KatCLIOpts
    -> ProbabilisticActionConfiguration
    -> Maybe (NetworkCapacity BellKATTag)
    -> ProbBellKATTest
    -> ProbBellKATPolicy
    -> IO ()
pbkatMainWithOpts = pbkatMainWithOpts' (Proxy @Probability)

pbkatMainWithOptsD
    :: KatCLIOpts
    -> ProbabilisticActionConfiguration
    -> Maybe (NetworkCapacity BellKATTag)
    -> ProbBellKATTest
    -> ProbBellKATPolicy
    -> IO ()
pbkatMainWithOptsD = pbkatMainWithOpts' (Proxy @Double)
