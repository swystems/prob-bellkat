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
import Data.Semigroup (stimes)

import BellKAT.Prelude
import BellKAT.DSL
import BellKAT.Definitions
import BellKAT.Definitions.Structures
import BellKAT.ActionEmbeddings (ProbabilisticActionConfiguration(..))
import BellKAT.Implementations.Configuration (NetworkCapacity, fromNetworkCapacity)
import BellKAT.Utils.Convex (CD, computeEventProbabilityRange)
import BellKAT.Utils.Distribution (RationalOrDouble)
import BellKAT.Prelude.Common
import BellKAT.Bundles.Probabilistic
import BellKAT.Bundles.Core

type ProbBellKATTest = BoundedTest BellKATTag
type ProbBellKATAction = TaggedAction BellKATTag

type ProbBellKATPolicy = OrderedGuardedPolicy ProbBellKATTest ProbBellKATAction


pbkatMain'
    :: (RationalOrDouble p, A.ToJSON p, A.FromJSON p)
    => Proxy p
    -> ProbabilisticActionConfiguration
    -> Maybe (NetworkCapacity BellKATTag)
    -> ProbBellKATTest
    -> ProbBellKATPolicy
    -> IO ()
pbkatMain' (_ :: Proxy p) pac mbNC ev protocol =
    let ep = fromNetworkCapacity mbNC
        runPipeline = probStarPolicyPipeline' @p pac ep mempty
        systemPipeline = probStarPolicySystemPipeline' @p pac ep mempty
        automatonPipeline = probStarPolicyAutomatonPipeline pac in do
    opts <- runKatParser "PBKAT tool"
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
          mbRStored :: Maybe (CD p (TaggedBellPairs tag)) <- A.decode <$> BS.getContents
          case mbRStored of
            Nothing -> error "Couldn't parse input"
            Just rStored ->
                let probRange = computeEventProbabilityRange (getBPsPredicate . toBPsPredicate  $ ev) rStored
                 in if kcoJSON opts
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
