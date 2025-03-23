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

import GHC.Exts (toList)
import Data.Default
import Data.Typeable
import Data.List (intercalate)
import Data.Ratio
import qualified Options.Applicative as OA
import Control.Monad

import BellKAT.Prelude (BellKATTag)
import BellKAT.DSL
import BellKAT.Definitions
import BellKAT.Definitions.Structures
import BellKAT.ActionEmbeddings (ProbabilisticActionConfiguration(..))
import BellKAT.Implementations.ProbAtomicOneStepQuantum (NetworkCapacity)
import BellKAT.Utils.Convex (CD')
import BellKAT.Utils.Distribution (D')

type ProbBellKATTest = BoundedTest BellKATTag
type ProbBellKATAction = TaggedAction BellKATTag

type ProbBellKATPolicy = OrderedGuardedPolicy ProbBellKATTest ProbBellKATAction

data PbkatCLIOpts = PCO 
    { pcoTex :: Bool
    , pcoCount :: Bool
    }

pcoParser :: OA.Parser PbkatCLIOpts
pcoParser = PCO 
    <$> OA.flag False True (OA.long "tex") 
    <*> OA.flag False True (OA.long "count")

pbkatMain :: (Typeable tag, Default tag, Show tag, Ord tag) 
          => CD' (TaggedBellPairs tag) -> IO ()
pbkatMain r = do
    opts <- OA.execParser $ OA.info pcoParser (OA.progDesc "PBKAT tool")
    when (pcoCount opts) $ 
        putStrLn $ "Num Generators: " <> show (length $ toList r)
    if pcoTex opts 
       then putStrLn $ showTeX r
       else print r

class ShowTeX a where
    showTeX :: a -> String

instance (Typeable tag, Default tag, Show tag, Ord tag) 
  => ShowTeX (CD' (TaggedBellPairs tag)) where
    showTeX cdbps = 
        "\\begin{gather*}\n" 
        <> "\\llparenthesis\n" 
        <> intercalate ",\\\\\n" [showTeX d | d <- toList cdbps ]
        <> "\n\\rrparenthesis\n"
        <> "\\end{gather*}\n"

instance (Typeable tag, Default tag, Show tag, Ord tag)
         => ShowTeX (D' (TaggedBellPairs tag)) where
    showTeX dbps = intercalate "+"
        [ "\\{\\!\\{" <> showTeX bps <> "\\}\\!\\}" <> "\\times" <> showTeX p
        | (bps, p) <- toList dbps ]

instance (Ord tag, Show tag, Default tag) => ShowTeX (TaggedBellPairs tag) where
    showTeX bps = intercalate "," [showTeX bp | bp <- toList bps]

instance Default tag => ShowTeX (TaggedBellPair tag) where
    showTeX tbp = let (l1, l2) = locations tbp in name l1 <> "\\!\\sim\\!" <> name l2

instance ShowTeX Probability where
    showTeX p = "\\frac{" <> show (numerator p) <> "}{" <> show (denominator p) <> "}"
