{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module BellKAT.QuantumPrelude (
    -- * QBKAT Policy syntax
    QBKATTag,
    QBKATRuntimeTag,
    NetworkState,
    QBKATTest,
    QBKATAction,
    QBKATPolicy,
    -- * Helpers
    createNetworkState,
    -- * Re-exports
    QuantumTag(..),
    TaggedBellPair(..),
    ProbabilisticActionConfiguration(..),
    Location,
    NetworkCapacity,
    MaxClock(..),
    -- * Network bounds
    CutoffSpec,
    NetworkBounds(..),
    def,
    -- * Entry points
    qbkatMain,
    qbkatMainD,
    qbkatMainWithOpts,
    qbkatMainWithOptsD,
    -- * Auxiliary expression generation exports
    stimes,
    -- * Re-exports from 'BellKAT.DSL'
    module BellKAT.DSL,
    (~),
    -- * Re-exports from 'BellKAT.Definitions.Structures'
    module BellKAT.Definitions.Structures,
) where

import Data.Typeable
import qualified Data.Aeson as A
import Data.Semigroup (stimes)
import Data.Default

import BellKAT.DSL
import BellKAT.Definitions
import BellKAT.Definitions.Atomic ()
import BellKAT.Definitions.Structures
import BellKAT.ActionEmbeddings (ProbabilisticActionConfiguration(..))
import BellKAT.Implementations.Configuration (NetworkCapacity, ExecutionParams(..))
import BellKAT.Implementations.Output (ListOutput, staticBellPairs, OutputBellPairs)
import BellKAT.Implementations.QuantumOps (QuantumOutput, QuantumTag(..), MaxClock(..), TimeUnit, isFresh)
import BellKAT.Prelude.Common
import BellKAT.Utils.Distribution (RationalOrDouble)
import qualified BellKAT.Utils.Multiset as Mset
import BellKAT.Bundles.OpBased
import BellKAT.Utils.Automata.Transitions.Functorial
import BellKAT.Utils.Convex
import qualified BellKAT.Implementations.GuardedAutomataStepQuantum as GASQ
import qualified BellKAT.Implementations.ProbAtomicOneStepQuantum as PAOSQ

type QBKATTag = ()
type QBKATRuntimeTag = QuantumTag

type QBKATTest = BoundedTest QBKATTag
type QBKATAction = TaggedAction QBKATTag

type QBKATPolicy = OrderedGuardedPolicy QBKATTest QBKATAction

type QBKATOutput = ListOutput QuantumOutput

type NetworkState = OutputBellPairs QuantumOutput

-- | Cutoff specification (time units for now, possibly extended with fidelity later)
type CutoffSpec = TimeUnit

-- | NetworkBounds with optional capacity and cutoff configuration
data NetworkBounds tag = NetworkBounds
    { nbCapacity :: Maybe (NetworkCapacity tag)
    , nbCutoff   :: Maybe CutoffSpec
    }

instance Default (NetworkBounds tag) where
    def = NetworkBounds { nbCapacity = Nothing, nbCutoff = Nothing }

-- | Build a 'NetworkState' (multiset of tagged Bell pairs) from list
createNetworkState :: [TaggedBellPair QBKATRuntimeTag] -> MaxClock -> NetworkState
createNetworkState bps tMax = Mset.fromList bps Mset.@ tMax

-- | Build an 'ExecutionParams' from 'NetworkBounds'
executionParamsFromNetworkBounds :: NetworkBounds t -> ExecutionParams t QuantumTag MaxClock
executionParamsFromNetworkBounds nb = EP
    { epNetworkCapacity = nbCapacity nb
    , epFilter          = \tbp clock -> isFresh tbp clock (nbCutoff nb)
    }

type QBKATSystem p = StateSystem (CD p) (OutputBellPairs QBKATOutput)
type QBKATAutomaton = GASQ.GuardedAutomatonStepQuantum QBKATTest (PAOSQ.ProbAtomicOneStepPolicy QBKATOutput QBKATTag)

-- | Prepares 'RunPipelines' for QBKAT.
createQuantumPipelines
    :: forall p. (RationalOrDouble p, A.ToJSON p, A.FromJSON p)
    => Proxy p
    -> ProbabilisticActionConfiguration
    -> NetworkBounds QBKATTag
    -> NetworkState
    -> RunPipelines p QBKATPolicy (QBKATSystem p) QBKATAutomaton NetworkState
createQuantumPipelines _ pac nb ns =
    let ep = executionParamsFromNetworkBounds nb
     in RunPipelines
        { runPipeline = probStarPolicyOpPipeline' @p (Proxy :: Proxy QBKATOutput) pac ep ns
        , systemPipeline = probStarPolicyOpSystemPipeline' @p (Proxy :: Proxy QBKATOutput) pac ep ns
        , automatonPipeline = probStarPolicyAutomatonPipeline (Proxy :: Proxy QBKATOutput) pac
        }

qbkatMain'
    :: forall p. (RationalOrDouble p, A.ToJSON p, A.FromJSON p)
    => Proxy p
    -> ProbabilisticActionConfiguration
    -> NetworkBounds QBKATTag
    -> QBKATTest
    -> QBKATPolicy
    -> NetworkState
    -> IO ()
qbkatMain' p pac nb ev protocol ns =
    let rp = createQuantumPipelines p pac nb ns
    in main rp "QBKAT tool" protocol ((. staticBellPairs) . testBellPairs  $ ev)

-- | speicialization of `pbkatMain'` to rational probability `Probability`
qbkatMain
    :: ProbabilisticActionConfiguration
    -> NetworkBounds QBKATTag
    -> QBKATTest
    -> QBKATPolicy
    -> NetworkState
    -> IO ()
qbkatMain = qbkatMain' (Proxy :: Proxy Probability)

-- | speicialization of `qbkatMain'` to floating point probability `Double`
qbkatMainD
    :: ProbabilisticActionConfiguration
    -> NetworkBounds QBKATTag
    -> QBKATTest
    -> QBKATPolicy
    -> NetworkState
    -> IO ()
qbkatMainD = qbkatMain' (Proxy :: Proxy Double)

qbkatMainWithOpts'
    :: forall p. (RationalOrDouble p, A.ToJSON p, A.FromJSON p)
    => Proxy p
    -> KatCLIOpts
    -> ProbabilisticActionConfiguration
    -> NetworkBounds QBKATTag
    -> QBKATTest
    -> QBKATPolicy
    -> NetworkState
    -> IO ()
qbkatMainWithOpts' p opts pac nb ev protocol ns =
    let rp = createQuantumPipelines p pac nb ns
    in mainWithOpts rp opts protocol ((. staticBellPairs) . testBellPairs  $ ev)

qbkatMainWithOpts
    :: KatCLIOpts
    -> ProbabilisticActionConfiguration
    -> NetworkBounds QBKATTag
    -> QBKATTest
    -> QBKATPolicy
    -> NetworkState
    -> IO ()
qbkatMainWithOpts = qbkatMainWithOpts' (Proxy @Probability)

qbkatMainWithOptsD
    :: KatCLIOpts
    -> ProbabilisticActionConfiguration
    -> NetworkBounds QBKATTag
    -> QBKATTest
    -> QBKATPolicy
    -> NetworkState
    -> IO ()
qbkatMainWithOptsD = qbkatMainWithOpts' (Proxy @Double)
