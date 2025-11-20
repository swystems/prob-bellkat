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
    -- * Auxiliary expression generation exports
    stimes,
    -- * Re-exports from 'BellKAT.DSL'
    module BellKAT.DSL,
    (~),
    -- * Re-exports from 'BellKAT.Definitions.Structures'
    module BellKAT.Definitions.Structures,
    -- * Re-exports from 'BellKAT.ActionEmbeddings
) where

import Data.Typeable
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Options.Applicative as OA
import Data.Semigroup (stimes)

import BellKAT.Prelude
import BellKAT.DSL
import BellKAT.Definitions
import BellKAT.Definitions.Structures
import BellKAT.ActionEmbeddings (ProbabilisticActionConfiguration(..))
import BellKAT.Implementations.ProbAtomicOneStepQuantum (NetworkCapacity, ExecutionParams(..))
import BellKAT.Utils.Convex (CD, computeEventProbabilityRange)
import BellKAT.Utils.Distribution (RationalOrDouble)

type ProbBellKATTest = BoundedTest BellKATTag
type ProbBellKATAction = TaggedAction BellKATTag

type ProbBellKATPolicy = OrderedGuardedPolicy ProbBellKATTest ProbBellKATAction

data PbkatMode = PMRun | PMTrace | PMProbability | PMAutomaton

data PbkatCLIOpts = PCO 
    { pcoJSON :: Bool
    , pcoMode :: PbkatMode
    }

-- | Default filter that keeps all bell pairs
noBellPairFilter :: TaggedBellPair tag -> () -> Bool
noBellPairFilter _ _ = True

pcoParser :: OA.Parser PbkatCLIOpts
pcoParser = PCO 
    <$> OA.flag False True (OA.long "json" <> OA.help "Generate JSON") 
    <*> OA.subparser (
            OA.command "run" 
                (OA.info (pure PMRun) (OA.progDesc "Run the procotol")) 
                <>
            OA.command "execution-trace"
                (OA.info (pure PMTrace) (OA.progDesc "Run the procotol")) 
                <>
            OA.command "automaton"
                (OA.info (pure PMAutomaton) (OA.progDesc "Run the procotol")) 
                <>
            OA.command "probability" 
                (OA.info (pure PMProbability) (OA.progDesc "Compute event probability"))
        )


pbkatMain' 
    :: (RationalOrDouble p, A.ToJSON p, A.FromJSON p) 
    => Proxy p
    -> ProbabilisticActionConfiguration 
    -> Maybe (NetworkCapacity BellKATTag)
    -> ProbBellKATTest
    -> ProbBellKATPolicy
    -> IO ()
pbkatMain' (_ :: Proxy p) pac mbNC ev protocol = 
    let ep = EP { networkCapacity = mbNC
                , bellPairFilter  = noBellPairFilter
                }
        r = applyProbStarPolicy' @p pac ep protocol mempty
        s = applyProbStarPolicySystem' @p pac ep protocol mempty
        a = applyProbStarPolicyAutomaton pac protocol in do
    opts <- OA.execParser $ OA.info (pcoParser OA.<**> OA.helper) (OA.fullDesc <> OA.progDesc "PBKAT tool")
    case pcoMode opts of
      PMRun ->
        if pcoJSON opts
           then BS.putStr $ A.encode r
           else print r
      PMTrace -> 
        print s
      PMAutomaton ->
        print a
      PMProbability -> do
          mbRStored :: Maybe (CD p (TaggedBellPairs tag)) <- A.decode <$> BS.getContents
          case mbRStored of 
            Nothing -> error "Couldn't parse input"
            Just rStored -> 
                let probRange = computeEventProbabilityRange (getBPsPredicate . toBPsPredicate  $ ev) rStored
                 in if pcoJSON opts
                       then BS.putStr $ A.encode probRange
                       else print probRange

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
