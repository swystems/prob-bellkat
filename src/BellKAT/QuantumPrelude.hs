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
import qualified Data.ByteString.Lazy as BS
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
import BellKAT.Utils.Convex (CD, computeEventProbabilityRange)
import BellKAT.Prelude.Common
import BellKAT.Utils.Distribution (RationalOrDouble)
import BellKAT.Utils.Multiset (LabelledMultiset)
import qualified BellKAT.Utils.Multiset as Mset
import BellKAT.Bundles.OpBased
import BellKAT.Bundles.Core

type QBKATTag = ()
type QBKATRuntimeTag = QuantumTag

type QBKATTest = BoundedTest QBKATTag
type QBKATAction = TaggedAction QBKATTag

type QBKATPolicy = OrderedGuardedPolicy QBKATTest QBKATAction

type QBKATOutput = ListOutput QuantumOutput

type NetworkState = LabelledMultiset MaxClock (TaggedBellPair QBKATRuntimeTag)

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


qbkatMain'
    :: (RationalOrDouble p, A.ToJSON p, A.FromJSON p)
    => Proxy p
    -> ProbabilisticActionConfiguration
    -> NetworkBounds QBKATTag
    -> QBKATTest
    -> QBKATPolicy
    -> NetworkState
    -> IO ()
qbkatMain' (_ :: Proxy p) pac nb ev protocol ns =
                                          {- ^ initial network state -}
    let ep = EP { epNetworkCapacity = nbCapacity nb
                , epFilter          = \tbp clock -> isFresh tbp clock (nbCutoff nb)
                }
        runPipeline = probStarPolicyOpPipeline' @p (Proxy :: Proxy QBKATOutput) pac ep ns
        systemPipeline = probStarPolicyOpSystemPipeline' @p (Proxy :: Proxy QBKATOutput) pac ep ns
        automatonPipeline = probStarPolicyAutomatonPipeline (Proxy :: Proxy QBKATOutput) pac in do
        opts <- runKatParser "QBKAT tool"
        case kcoMode opts of
          KMRun -> do
              r <- runLoggedPipeline runPipeline protocol
              if kcoJSON opts
                 then BS.putStr $ A.encode r
                 else print r
          KMTrace ->
              runLoggedPipeline systemPipeline protocol >>= print
          KMAutomaton ->
              runLoggedPipeline automatonPipeline protocol >>= print
          KMProbability -> do
              mbRStored :: Maybe (CD p (OutputBellPairs QBKATOutput)) <- A.decode <$> BS.getContents
              case mbRStored of
                Nothing -> error "Couldn't parse input"
                Just rStored ->
                    let probRange = computeEventProbabilityRange ((. staticBellPairs) . getBPsPredicate . toBPsPredicate  $ ev) rStored
                     in if kcoJSON opts
                           then BS.putStr $ A.encode probRange
                           else print probRange


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
