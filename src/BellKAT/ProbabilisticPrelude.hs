{-# LANGUAGE OverloadedStrings #-}
module BellKAT.ProbabilisticPrelude (
    BellKATTagChar(..),
    BellKATTag,
    ProbBellKATTest,
    ProbBellKATAction,
    ProbBellKATPolicy,
    ProbabilisticActionConfiguration(..),
    NetworkCapacity,
    applyProbStarPolicy,
    applyProbStarPolicyD,
    pbkatMain,
    -- * Auxiliary expression generation exports
    stimes,
    -- * Re-exports from 'BellKAT.DSL'
    module BellKAT.DSL,
    (~),
    -- * Re-exports from 'BellKAT.Definitions.Structures'
    module BellKAT.Definitions.Structures,
    -- * Re-exports from 'BellKAT.ActionEmbeddings
) where

import Data.Default
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
import BellKAT.Implementations.ProbAtomicOneStepQuantum (NetworkCapacity)
import BellKAT.Utils.Convex (CD, computeEventProbabilityRange)
import BellKAT.Utils.Distribution (RationalOrDouble)

type ProbBellKATTest = BoundedTest BellKATTag
type ProbBellKATAction = TaggedAction BellKATTag

type ProbBellKATPolicy = OrderedGuardedPolicy ProbBellKATTest ProbBellKATAction

applyProbStarPolicyD 
    :: (Typeable tag, Ord tag, Show tag, Default tag, DecidableBoolean (test tag), Test test, Show (test tag)) 
    => ProbabilisticActionConfiguration 
    -> Maybe (NetworkCapacity tag)
    -> Simple (OrderedGuardedPolicy (test tag)) tag 
    -> TaggedBellPairs tag -> CD Double (TaggedBellPairs tag)
applyProbStarPolicyD = applyProbStarPolicy'

data PbkatMode = PMRun | PMProbability

data PbkatCLIOpts = PCO 
    { pcoJSON :: Bool
    , pcoMode :: PbkatMode
    }

pcoParser :: OA.Parser PbkatCLIOpts
pcoParser = PCO 
    <$> OA.flag False True (OA.long "json" <> OA.help "Generate JSON") 
    <*> OA.subparser (
            OA.command "run" 
                (OA.info (pure PMRun) (OA.progDesc "Run the procotol")) 
                <>
            OA.command "probability" 
                (OA.info (pure PMProbability) (OA.progDesc "Compute event probability"))
        )


pbkatMain 
    :: (Typeable tag, Default tag, Show tag, Ord tag, RationalOrDouble p, A.ToJSON p, A.FromJSON p) 
    => CD p (TaggedBellPairs tag) -> BellPairsPredicate tag -> IO ()
pbkatMain (r :: CD p (TaggedBellPairs tag)) ev = do
    opts <- OA.execParser $ OA.info (pcoParser OA.<**> OA.helper) (OA.fullDesc <> OA.progDesc "PBKAT tool")
    case pcoMode opts of
      PMRun ->
        if pcoJSON opts
           then BS.putStr $ A.encode r
           else print r
      PMProbability -> do
          mbRStored :: Maybe (CD p (TaggedBellPairs tag)) <- A.decode <$> BS.getContents
          case mbRStored of 
            Nothing -> error "Couldn't parse input"
            Just rStored -> 
                let probRange = computeEventProbabilityRange (getBPsPredicate ev) rStored
                 in if pcoJSON opts
                       then BS.putStr $ A.encode probRange
                       else print probRange
