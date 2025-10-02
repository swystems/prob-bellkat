{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Implementations.Output (
    Output(..),
    OpOutput(..),
    RuntimeTag(..),
    staticBellPair,
    staticBellPairs,
    ) where

import Data.Kind
import Data.Default
import Control.Subcategory.Pointed
import Control.Subcategory.Functor
import Control.Subcategory.Bind
import GHC.Exts (fromList, toList)

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Definitions.Core
import BellKAT.Utils.Distribution as D hiding (Probability)
import BellKAT.Utils.Choice
import BellKAT.Utils.Convex

class RuntimeTag rTag tag where
    staticTag :: rTag -> tag

instance RuntimeTag tag tag where
    staticTag = id

-- | Remove runtime tags from a TaggedBellPair, yielding the underlying BellPair
staticBellPair :: (RuntimeTag rTag tag) => TaggedBellPair rTag -> TaggedBellPair tag
staticBellPair = fmap staticTag

-- | Remove runtime tags from a multiset of tagged Bell pairs
staticBellPairs :: (RuntimeTag rTag tag, Ord rTag, Ord tag) => TaggedBellPairs rTag -> TaggedBellPairs tag
staticBellPairs = Mset.map staticBellPair


class (RuntimeTag (RTag output) tag) => Output output tag | output -> tag where
    type RTag output :: Type
    computeOutput :: output -> TaggedBellPairs (RTag output) -> CD' (TaggedBellPairs (RTag output))

class Output output tag => OpOutput output op tag | output -> tag where
    fromCBPOutput :: TaggedBellPair tag -> op -> output

newtype ListOutput output tag = ListOutput [(TaggedBellPairs tag, output)] 
    deriving newtype (Semigroup, Monoid)

-- TODO: fix undecideable instance caused by Ord (RTag) 
instance (Ord tag, DDom (RTag output), Default (RTag output), Output output tag) 
        => Output (ListOutput output tag) tag where
    type RTag (ListOutput output tag) = (RTag output)
    computeOutput (ListOutput xs) = computeOutputHelper xs
      where
        computeOutputHelper [] untouched = cpure untouched
        computeOutputHelper ((i, o):ios) bps = 
            mconcat
                [ cjoin $ cmap (\x -> cmap (x <>) tl) (computeOutput o (chosen partial)) 
                | partial <- findElemsND' staticBellPair (toList i) bps
                , let tl = computeOutputHelper ios (rest partial)
                ]    

instance (DDom tag, Default tag) => Output (D' (TaggedBellPairs tag)) tag where
    type RTag (D' (TaggedBellPairs tag)) = tag
    computeOutput x _ = fromList [x]

instance (DDom tag, Default tag) => OpOutput (D' (TaggedBellPairs tag)) Probability tag where
    fromCBPOutput o p
      | p == 1 = cpure (Mset.singleton o)
      | p == 0 = cpure mempty
      | otherwise = D.choose p (Mset.singleton o) mempty

