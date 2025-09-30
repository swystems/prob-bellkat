module BellKAT.Implementations.Output where

import Data.Kind
import Data.Default
import Control.Subcategory.Pointed
import GHC.Exts (fromList)

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Definitions.Core
import BellKAT.Utils.Distribution hiding (Probability)
import BellKAT.Utils.Convex

class RuntimeTag rTag tag where
    staticTag :: rTag -> tag

instance RuntimeTag tag tag where
    staticTag = id

class (RuntimeTag (RTag output) tag, Monoid output) => Output output tag | output -> tag where
    type RTag output :: Type
    computeOutput :: output -> TaggedBellPairs (RTag output) -> CD' (TaggedBellPairs (RTag output))

class Output output tag => OpOutput output op tag | output -> tag where
    fromCBPOutput :: TaggedBellPair tag -> op -> output

instance (DDom tag, Default tag) => Output (D' (TaggedBellPairs tag)) tag where
    type RTag (D' (TaggedBellPairs tag)) = tag
    computeOutput x _ = fromList [x]

instance (DDom tag, Default tag) => OpOutput (D' (TaggedBellPairs tag)) Probability tag where
    fromCBPOutput o p
      | p == 1 = cpure (Mset.singleton o)
      | p == 0 = cpure mempty
      | otherwise = choose p (Mset.singleton o) mempty

