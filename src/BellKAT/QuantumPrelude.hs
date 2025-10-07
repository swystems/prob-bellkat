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
    NetworkCapacity,
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
import qualified Options.Applicative as OA
import Data.Semigroup (stimes)

import BellKAT.DSL
import BellKAT.Definitions
import BellKAT.Definitions.Atomic ()  
import BellKAT.Definitions.Structures
import BellKAT.ActionEmbeddings (ProbabilisticActionConfiguration(..))
import BellKAT.Implementations.ProbAtomicOneStepQuantum (NetworkCapacity)
import BellKAT.Implementations.Output (ListOutput, OpOutput, RTag, staticBellPairs)
import BellKAT.Utils.Convex (CD, computeEventProbabilityRange)
import BellKAT.Utils.Distribution (RationalOrDouble)
import BellKAT.Implementations.QuantumOps
import qualified BellKAT.Utils.Multiset as Mset

type QBKATTag = ()
type QBKATRuntimeTag = QuantumTag

type QBKATTest = BoundedTest QBKATTag
type QBKATAction = TaggedAction QBKATTag

type QBKATPolicy = OrderedGuardedPolicy QBKATTest QBKATAction

type QBKATOutput = ListOutput (TaggedBellPair (), Op QBKATRuntimeTag) QBKATTag

type NetworkState = TaggedBellPairs QBKATRuntimeTag

-- | Build a 'NetworkState' (multiset of tagged Bell pairs) from list
createNetworkState :: [TaggedBellPair QBKATRuntimeTag] -> NetworkState
createNetworkState = Mset.fromList

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
    -> Maybe (NetworkCapacity QBKATTag)
    -> QBKATTest
    -> QBKATPolicy
    -> TaggedBellPairs QBKATRuntimeTag
    -> IO ()
qbkatMain' (_ :: Proxy p) pac mbNC ev protocol ns = 
                                            {- ^ initial network state -}  
    let r = applyProbStarPolicyQ' @p (Proxy :: Proxy QBKATOutput) pac mbNC protocol ns
        s = applyProbStarPolicyQSystem' @p (Proxy :: Proxy QBKATOutput) pac mbNC protocol ns
        a = applyProbStarPolicyQAutomaton (Proxy :: Proxy QBKATOutput) pac protocol in do
    opts <- OA.execParser $ OA.info (qcoParser OA.<**> OA.helper) (OA.fullDesc <> OA.progDesc "QBKAT tool")
    case qcoMode opts of
      QMRun ->
        if qcoJSON opts
           then BS.putStr $ A.encode r
           else print r
      QMTrace -> 
        print s
      QMAutomaton ->
        print a
      QMProbability -> do
          mbRStored :: Maybe (CD p (TaggedBellPairs (RTag QBKATOutput))) <- A.decode <$> BS.getContents
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
    -> Maybe (NetworkCapacity QBKATTag)
    -> QBKATTest
    -> QBKATPolicy
    -> TaggedBellPairs QBKATRuntimeTag
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
    -> Maybe (NetworkCapacity QBKATTag)
    -> QBKATTest
    -> QBKATPolicy
    -> TaggedBellPairs QBKATRuntimeTag
    -> IO ()
qbkatMainD = qbkatMain' (Proxy :: Proxy Double)
