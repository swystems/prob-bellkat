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

newtype PbkatCLIOpts = PCO 
    { pcoJSON :: Bool
    }

pcoParser :: OA.Parser PbkatCLIOpts
pcoParser = PCO 
    <$> OA.flag False True (OA.long "json") 

pbkatMain :: (Typeable tag, Default tag, Show tag, Ord tag, RationalOrDouble p, A.ToJSON p) 
          => CD p (TaggedBellPairs tag) -> BellPairsPredicate tag -> IO ()
pbkatMain r ev = do
    opts <- OA.execParser $ OA.info pcoParser (OA.progDesc "PBKAT tool")
    let probRange = computeEventProbabilityRange (getBPsPredicate ev) r
    if pcoJSON opts
       then BS.putStr $ A.encode $ A.object ["output" A..= A.toJSON r, "probability" A..= A.toJSON probRange ] 
        else print r
