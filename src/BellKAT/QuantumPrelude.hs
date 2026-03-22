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
    while,
    (~),
    -- * Re-exports from 'BellKAT.Definitions.Structures'
    module BellKAT.Definitions.Structures,
) where

import Data.Typeable
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Options.Applicative as OA
import Control.Monad.Logger (runStderrLoggingT)
import Data.Semigroup (stimes)
import Data.Default

import BellKAT.DSL
import BellKAT.Definitions
import BellKAT.Bundles.Guarded
import BellKAT.Definitions.Atomic ()  
import BellKAT.Definitions.Structures
import BellKAT.ActionEmbeddings (ProbabilisticActionConfiguration(..))
import BellKAT.Implementations.Configuration (NetworkCapacity, ExecutionParams(..))
import BellKAT.Implementations.Output (ListOutput, OpOutput, staticBellPairs, OutputBellPairs)
import BellKAT.Implementations.QuantumOps (QuantumOutput, QuantumTag(..), MaxClock(..), TimeUnit, isFresh)
import BellKAT.Utils.Convex (CD, computeEventProbabilityRange)
import BellKAT.Utils.Distribution (RationalOrDouble)
import BellKAT.Utils.Multiset (LabelledMultiset)
import qualified BellKAT.Utils.Multiset as Mset
import BellKAT.Bundles.Core

type QBKATTag = ()
type QBKATRuntimeTag = QuantumTag

type QBKATTest = BoundedTest QBKATTag
type QBKATAction = TaggedAction QBKATTag

type QBKATPolicy = OrderedGuardedPolicy QBKATTest QBKATAction

type QBKATOutput = ListOutput QuantumOutput MaxClock QBKATTag

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

runLoggedPipeline :: Pipeline a b -> a -> IO b
runLoggedPipeline p input = runStderrLoggingT $ executePipeline p input


-- | Build a 'NetworkState' (multiset of tagged Bell pairs) from list
createNetworkState :: [TaggedBellPair QBKATRuntimeTag] -> MaxClock -> NetworkState
createNetworkState bps tMax = Mset.fromList bps Mset.@ tMax

data QbkatMode = QMRun | QMTrace | QMProbability | QMAutomaton

data QbkatCLIOpts = QCO 
    { qcoJSON :: Bool
    , qcoMode :: QbkatMode
    }

qcoParser :: OA.Parser QbkatCLIOpts
qcoParser = QCO 
    <$> OA.flag False True (OA.long "json" <> OA.help "Generate JSON") 
    <*> OA.subparser (
            OA.command "run" 
                (OA.info (pure QMRun) (OA.progDesc "Run the procotol")) 
                <>
            OA.command "execution-trace"
                (OA.info (pure QMTrace) (OA.progDesc "Run the procotol")) 
                <>
            OA.command "automaton"
                (OA.info (pure QMAutomaton) (OA.progDesc "Run the procotol")) 
                <>
            OA.command "probability" 
                (OA.info (pure QMProbability) (OA.progDesc "Compute event probability"))
        )


qbkatMain' 
    :: (RationalOrDouble p, A.ToJSON p, A.FromJSON p
        , DecidableBoolean QBKATTest
        , Show QBKATTest
        , Show QBKATOutput
        , Ord QBKATOutput
        , OpOutput QBKATOutput (Op QBKATRuntimeTag) QBKATTag 
    )
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
        runPipeline = probStarPolicyQPipeline' @p (Proxy :: Proxy QBKATOutput) pac ep ns
        systemPipeline = probStarPolicyQSystemPipeline' @p (Proxy :: Proxy QBKATOutput) pac ep ns
        automatonPipeline = probStarPolicyAutomatonPipeline (Proxy :: Proxy QBKATOutput) pac in do
        opts <- OA.execParser $ OA.info (qcoParser OA.<**> OA.helper) (OA.fullDesc <> OA.progDesc "QBKAT tool")
        case qcoMode opts of
          QMRun -> do
              r <- runLoggedPipeline runPipeline protocol
              if qcoJSON opts
                 then BS.putStr $ A.encode r
                 else print r
          QMTrace -> 
              runLoggedPipeline systemPipeline protocol >>= print
          QMAutomaton ->
              runLoggedPipeline automatonPipeline protocol >>= print
          QMProbability -> do
              mbRStored :: Maybe (CD p (OutputBellPairs QBKATOutput)) <- A.decode <$> BS.getContents
              case mbRStored of 
                Nothing -> error "Couldn't parse input"
                Just rStored -> 
                    let probRange = computeEventProbabilityRange ((. staticBellPairs) . getBPsPredicate . toBPsPredicate  $ ev) rStored
                     in if qcoJSON opts
                           then BS.putStr $ A.encode probRange
                           else print probRange


-- | speicialization of `pbkatMain'` to rational probability `Probability`
qbkatMain 
    :: ( DecidableBoolean QBKATTest
       , Show QBKATTest
       , Show QBKATOutput
       , Ord QBKATOutput
       , OpOutput QBKATOutput (Op QBKATRuntimeTag) QBKATTag
       )
    => ProbabilisticActionConfiguration 
    -> NetworkBounds QBKATTag
    -> QBKATTest
    -> QBKATPolicy
    -> NetworkState
    -> IO ()
qbkatMain = qbkatMain' (Proxy :: Proxy Probability)

-- | speicialization of `qbkatMain'` to floating point probability `Double`
qbkatMainD 
    :: ( DecidableBoolean QBKATTest
       , Show QBKATTest
       , Show QBKATOutput
       , Ord QBKATOutput
       , OpOutput QBKATOutput (Op QBKATRuntimeTag) QBKATTag
       )
    => ProbabilisticActionConfiguration 
    -> NetworkBounds QBKATTag
    -> QBKATTest
    -> QBKATPolicy
    -> NetworkState
    -> IO ()
qbkatMainD = qbkatMain' (Proxy :: Proxy Double)
