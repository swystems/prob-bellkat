{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Implementations.Output (
    Output(..),
    OpOutput(..),
    ListOutput(..),
    RuntimeTag(..),
    staticBellPair,
    staticBellPairs,
    ) where

import Data.Kind
import Data.Default
import Data.Data
import Control.Subcategory.Pointed
import Control.Subcategory.Functor
import Control.Subcategory.Bind
import GHC.Exts (fromList, toList, IsList, Item)

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

-- | Remove runtime tags and MS label from a multiset of tagged Bell pairs
staticBellPairs :: (RuntimeTag rTag tag, Ord rTag, Ord tag) => LabelledBellPairs cTag rTag -> TaggedBellPairs tag
staticBellPairs = Mset.map' staticBellPair


class (RuntimeTag (RTag output) tag) => Output output tag | output -> tag where
    type RTag output :: Type
    type CTag output :: Type
    computeOutput :: output
                  -> LabelledBellPairs (CTag output) (RTag output)
                  -> CD' (LabelledBellPairs (CTag output) (RTag output))

class Output output tag => OpOutput output op tag | output -> tag where
    fromCBPOutput :: TaggedBellPairs tag -> TaggedBellPair tag -> op -> output

newtype ListOutput output cTag tag = ListOutput { unListOutput :: [(LabelledBellPairs cTag tag, output)] }
    deriving newtype (Semigroup, Monoid, Show, Ord, Eq)

instance IsList (ListOutput output cTag tag) where
    type Item (ListOutput output cTag tag) = (LabelledBellPairs cTag tag, output)
    toList = unListOutput
    fromList = ListOutput

-- TODO: fix undecideable instance caused by Ord (RTag) 
instance (Ord tag, DDom (RTag output), Default (RTag output), Output output tag,
          Semigroup (CTag output), Ord (CTag output), Typeable (CTag output), Show (CTag output))
        => Output (ListOutput output cTag tag) tag where
    type RTag (ListOutput output cTag tag) = (RTag output)
    type CTag (ListOutput output cTag tag) = (CTag output)
    computeOutput (ListOutput xs) = computeOutputHelper xs
      where
        computeOutputHelper [] untouched = cpure untouched
        computeOutputHelper ((i, o):ios) bps = 
            mconcat
                [ cjoin $ cmap (\x -> cmap (x <>) tl) (computeOutput o (chosen partial)) 
                | partial <- findElemsNDT staticBellPair (toList (Mset.bellPairs i)) bps
                , let tl = computeOutputHelper ios (rest partial)
                ]    

instance (Ord tag, DDom (RTag output), Default (RTag output), OpOutput output op tag
         , Monoid cTag
         , Semigroup (CTag output), Ord (CTag output), Typeable (CTag output), Show (CTag output)) 
         => OpOutput (ListOutput output cTag tag) op tag where
        fromCBPOutput i@(Mset.LMS (ms, ())) o p =
            fromList [(Mset.LMS (ms, mempty), fromCBPOutput i o p)]

instance (DDom tag, Default tag, Semigroup cTag, Show cTag, Ord cTag, Typeable cTag) 
    => Output (D' (LabelledBellPairs cTag tag)) tag where
    type RTag (D' (LabelledBellPairs cTag tag)) = tag
    type CTag (D' (LabelledBellPairs cTag tag)) = cTag
    computeOutput x _ = fromList [x]

instance (DDom tag, Default tag) => OpOutput (D' (LabelledBellPairs () tag)) Probability tag where
    fromCBPOutput _ o p
      | p == 1 = cpure (Mset.singleton' o)
      | p == 0 = cpure mempty
      | otherwise = D.choose p (Mset.singleton' o) mempty

