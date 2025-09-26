module BellKAT.Implementations.Output where

import Control.Subcategory.Pointed

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Definitions.Core
import BellKAT.Utils.Distribution hiding (Probability)

class Monoid output => Output output tag | output -> tag where
    computeOutput :: output -> TaggedBellPairs tag -> D' (TaggedBellPairs tag)
    fromProbabilisticBP :: TaggedBellPair tag -> Probability -> output

instance Ord tag => Output (D' (TaggedBellPairs tag)) tag where
    computeOutput x _ = x
    fromProbabilisticBP o p
      | p == 1 = cpure (Mset.singleton o)
      | p == 0 = cpure mempty
      | otherwise = choose p (Mset.singleton o) mempty

