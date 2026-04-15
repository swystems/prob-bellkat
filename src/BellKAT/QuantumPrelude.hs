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
import qualified Options.Applicative as OA
import Data.Semigroup (stimes)
import Data.Default
import           GHC.Exts (toList)

import BellKAT.DSL
import BellKAT.Definitions
import BellKAT.Definitions.Atomic ()
import BellKAT.Definitions.Structures
import BellKAT.ActionEmbeddings (ProbabilisticActionConfiguration(..))
import BellKAT.Implementations.Configuration (NetworkCapacity, ExecutionParams(..))
import BellKAT.Implementations.MDPProbability
    ( holdsStaticTest
    , toStaticBellPairs
    )
import BellKAT.Implementations.MDPWerner
    ( WernerBellPairs
    , toWernerBellPairs
    , holdsWernerTest
    )
import BellKAT.Implementations.ProbabilisticQuantumOps
    ( StateKind(..)
    )
import BellKAT.Implementations.MDPExtremal
    ( ExtremalQuery(..)
    , computeExtremalReachability
    , renderExtremalResult
    )
import BellKAT.Implementations.Output (ListOutput, staticBellPairs, OutputBellPairs)
import BellKAT.Implementations.QuantumOps (QuantumOutput, QuantumTag(..), MaxClock(..), TimeUnit, isFresh)
import BellKAT.Utils.Convex (CD, computeEventProbabilityRange)
import BellKAT.Utils.Distribution (RationalOrDouble)
import BellKAT.Utils.Multiset (LabelledMultiset)
import qualified BellKAT.Utils.Multiset as Mset
import BellKAT.Bundles.OpBased
import BellKAT.Bundles.Core

type QBKATTag = ()
type QBKATRuntimeTag = QuantumTag

type QBKATTest = KindedTest QBKATTag
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

data MDPCLIOpts = MDPCLIOpts
    { mcoComputeExtremal :: Bool
    , mcoCoverage :: Maybe Double
    , mcoBudget :: Maybe Int
    }

data QbkatMode = QMRun | QMTrace | QMProbability | QMAutomaton | QMMDP MDPCLIOpts | QMQMDP MDPCLIOpts

data QbkatCLIOpts = QCO
    { qcoJSON :: Bool
    , qcoMode :: QbkatMode
    }

mdpCLIParser :: OA.Parser MDPCLIOpts
mdpCLIParser =
    MDPCLIOpts
        <$> OA.flag False True
            ( OA.long "compute-extremal"
                <> OA.help "Compute extremal cost-bounded reachability from the derived MDP"
            )
        <*> OA.optional
            ( OA.option OA.auto
                ( OA.long "coverage"
                    <> OA.metavar "P"
                    <> OA.help "Stop when the worst-scheduler CDF at the initial state reaches probability P"
                )
            )
        <*> OA.optional
            ( OA.option OA.auto
                ( OA.long "truncation"
                    <> OA.metavar "R"
                    <> OA.help "Compute the extremal CDFs up to budget R"
                )
            )

resolveExtremalQuery :: MDPCLIOpts -> Either String (Maybe ExtremalQuery)
resolveExtremalQuery MDPCLIOpts{mcoComputeExtremal, mcoCoverage, mcoBudget}
    | hasCoverage && hasBudget =
        Left "Use either --coverage or --truncation, not both."
    | not wantsExtremal =
        Right Nothing
    | otherwise =
        case (mcoCoverage, mcoBudget) of
            (Just coverage, Nothing) -> Right . Just $ ExtremalCoverage coverage
            (Nothing, Just budget) -> Right . Just $ ExtremalBudget budget
            _ ->
                Left "Pass either --coverage or --truncation when requesting --compute-extremal."
  where
    hasCoverage = maybe False (const True) mcoCoverage
    hasBudget = maybe False (const True) mcoBudget
    wantsExtremal = mcoComputeExtremal || hasCoverage || hasBudget

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
                <>
            OA.command "mdp"
                (OA.info (QMMDP <$> mdpCLIParser) (OA.progDesc "Build or solve the time-only MDP"))
                <>
            OA.command "qmdp"
                (OA.info (QMQMDP <$> mdpCLIParser) (OA.progDesc "Build or solve the quality-aware MDP"))
        )


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
        qmdpEp = EP { epNetworkCapacity = nbCapacity nb
                    , epFilter          = \_ _ -> True
                    }
        runPipeline = probStarPolicyOpPipeline' @p (Proxy :: Proxy QBKATOutput) pac ep ns
        systemPipeline = probStarPolicyOpSystemPipeline' @p (Proxy :: Proxy QBKATOutput) pac ep ns
        mdpPipeline = probStarPolicyQMDPPipeline' @p pac ep (toStaticBellPairs ns)
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
          QMMDP mdpOpts -> do
              let mdp = runNonLoggedPipeline mdpPipeline protocol
              case resolveExtremalQuery mdpOpts of
                Left err ->
                    ioError (userError err)
                Right Nothing ->
                    if qcoJSON opts
                       then BS.putStr . A.encode $
                            A.object ["mdp_rendered" A..= show mdp]
                       else putStrLn (show mdp)
                Right (Just query) ->
                    case computeExtremalReachability (holdsStaticTest ev) query mdp of
                        Left err ->
                            ioError (userError err)
                        Right result ->
                            if qcoJSON opts
                               then BS.putStr . A.encode $
                                    A.object
                                        [ "mdp_rendered" A..= show mdp
                                        , "extremal" A..= result
                                        ]
                               else do
                                    putStrLn (show mdp)
                                    putStrLn ""
                                    putStrLn (renderExtremalResult result)
          QMQMDP mdpOpts -> do
              initialQState <-
                  case initialWernerState ns of
                      Left err -> ioError (userError err)
                      Right st -> pure st
              let qmdp = runNonLoggedPipeline (probStarPolicyWMDPPipeline' @p pac qmdpEp initialQState) protocol
              case resolveExtremalQuery mdpOpts of
                Left err ->
                    ioError (userError err)
                Right Nothing ->
                    if qcoJSON opts
                       then BS.putStr . A.encode $
                            A.object ["mdp_rendered" A..= show qmdp]
                       else putStrLn (show qmdp)
                Right (Just query) ->
                    case computeExtremalReachability (holdsWernerTest ev) query qmdp of
                        Left err ->
                            ioError (userError err)
                        Right result ->
                            if qcoJSON opts
                               then BS.putStr . A.encode $
                                    A.object
                                        [ "mdp_rendered" A..= show qmdp
                                        , "extremal" A..= result
                                        ]
                               else do
                                    putStrLn (show qmdp)
                                    putStrLn ""
                                    putStrLn (renderExtremalResult result)
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

initialWernerState :: NetworkState -> Either String WernerBellPairs
initialWernerState (Mset.LMS (bps, _)) =
    fmap (toWernerBellPairs . (Mset.@ ()) . Mset.fromList) . traverse toKindedPair . toList $ bps
  where
    toKindedPair (TaggedBellPair bp (QuantumTag _ w))
        | approx01 w 1 = Right (TaggedBellPair bp Pure)
        | approx01 w 0 = Right (TaggedBellPair bp Mixed)
        | otherwise =
            Left $
                "qmdp currently requires an initial state whose Bell pairs are already "
                <> "deterministically pure (Werner 1) or mixed (Werner 0); "
                <> "encountered Werner " <> show w <> " for " <> show bp

approx01 :: Double -> Double -> Bool
approx01 x y = abs (x - y) <= 1e-12


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
