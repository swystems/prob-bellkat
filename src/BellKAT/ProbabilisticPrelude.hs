module BellKAT.ProbabilisticPrelude (
    BellKATTag,
    ProbBellKATTest,
    ProbBellKATAction,
    ProbBellKATPolicy,
    ProbabilisticActionConfiguration(..),
    NetworkCapacity,
    applyProbStarPolicy,
    pbkatMain,
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

import BellKAT.Prelude (BellKATTag)
import BellKAT.DSL
import BellKAT.Definitions
import BellKAT.Definitions.Structures
import BellKAT.ActionEmbeddings (ProbabilisticActionConfiguration(..))
import BellKAT.Implementations.ProbAtomicOneStepQuantum (NetworkCapacity)
import BellKAT.Utils.Convex (CD')

type ProbBellKATTest = BoundedTest BellKATTag
type ProbBellKATAction = TaggedAction BellKATTag

type ProbBellKATPolicy = OrderedGuardedPolicy ProbBellKATTest ProbBellKATAction

newtype PbkatCLIOpts = PCO 
    { pcoJSON :: Bool
    }

pcoParser :: OA.Parser PbkatCLIOpts
pcoParser = PCO 
    <$> OA.flag False True (OA.long "json") 

pbkatMain :: (Typeable tag, Default tag, Show tag, Ord tag) 
          => CD' (TaggedBellPairs tag) -> IO ()
pbkatMain r = do
    opts <- OA.execParser $ OA.info pcoParser (OA.progDesc "PBKAT tool")
    if pcoJSON opts
        then BS.putStr $ A.encode r
        else print r
