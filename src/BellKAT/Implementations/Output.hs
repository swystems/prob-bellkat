module BellKAT.Implementations.Output where

import Data.Kind
import Control.Subcategory.Pointed

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Definitions.Core
import BellKAT.Utils.Distribution hiding (Probability)

class RuntimeTag rTag tag where
    staticTag :: rTag -> tag

instance RuntimeTag tag tag where
    staticTag = id

class (RuntimeTag (RTag output) tag, Monoid output) => Output output tag | output -> tag where
    type RTag output :: Type
    computeOutput :: output -> TaggedBellPairs (RTag output) -> D' (TaggedBellPairs (RTag output))
    fromProbabilisticBP :: TaggedBellPair tag -> Probability -> output

instance Ord tag => Output (D' (TaggedBellPairs tag)) tag where
    type RTag (D' (TaggedBellPairs tag)) = tag
    computeOutput x _ = x
    fromProbabilisticBP o p
      | p == 1 = cpure (Mset.singleton o)
      | p == 0 = cpure mempty
      | otherwise = choose p (Mset.singleton o) mempty

