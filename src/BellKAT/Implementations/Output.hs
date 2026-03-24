{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
module BellKAT.Implementations.Output (
    Output(..),
    OutputBellPairs,
    OpOutput(..),
    ListOutput(..),
    RuntimeTag(..),
    staticBellPair,
    staticBellPairs,
    ) where

import Data.Kind
import Data.Default
import Control.Subcategory.Pointed
import Control.Subcategory.Functor
import Control.Subcategory.Bind
import GHC.Exts (fromList, toList, IsList, Item)

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Definitions.Core
import BellKAT.Utils.Distribution as D hiding (Probability)
import BellKAT.Utils.Choice
import BellKAT.Utils.Convex

-- | RuntimeTag has at least enough information to recover the static tag
class (Tag rTag, Tag tag) => RuntimeTag rTag tag where
    staticTag :: rTag -> tag

instance Tag tag => RuntimeTag tag tag where
    staticTag = id

-- | Remove runtime tags from a TaggedBellPair, yielding the underlying BellPair
staticBellPair :: (RuntimeTag rTag tag) => TaggedBellPair rTag -> TaggedBellPair tag
staticBellPair = fmap staticTag

-- | Remove runtime tags and MS label from a multiset of tagged Bell pairs
staticBellPairs :: RuntimeTag rTag tag => LabelledBellPairs cTag rTag -> TaggedBellPairs tag
staticBellPairs = Mset.map' staticBellPair

-- ^ Constraints on the monoid structure of `OutputM`
type OutputDom a b = (Foldable a, CPointed a, CBind a, Dom a b, Dom a (a b), Monoid (a b))

-- ^ Convenience type class to quickly get output's BellPairs type
type OutputBellPairs output = LabelledBellPairs (CTag output) (RTag output)

-- ^ Shorthand for checking well-formedness of `OutputM` associated type  of `Output`
type WellFormedOutputM output = OutputDom (OutputM output) (OutputBellPairs output)

class (WellFormedOutputM output, Show output, Ord output, 
        RuntimeTag (RTag output) (STag output), Tag (CTag output)) => Output output  where
    type STag output :: Type
    type RTag output :: Type
    type CTag output :: Type
    type OutputM output :: Type -> Type
    computeOutput :: output
                  -> OutputBellPairs output
                  -> OutputM output (OutputBellPairs output)


class Output output => OpOutput output op where
    fromCBPOutput :: TaggedBellPairs (STag output) -> TaggedBellPair (STag output) -> op -> output

newtype ListOutput output = ListOutput { unListOutput :: [(LabelledBellPairs (CTag output) (STag output), output)] }
    deriving newtype (Semigroup, Monoid)

deriving newtype instance (Eq output, Eq (STag output), Eq (CTag output)) => Eq (ListOutput output)
deriving newtype instance (Ord output, Ord (STag output), Ord (CTag output)) => Ord (ListOutput output)
deriving newtype instance (Default (STag output), Eq (STag output), Show output, Show (STag output), Show (CTag output)) 
    => Show (ListOutput output)

instance IsList (ListOutput output) where
    type Item (ListOutput output) = (LabelledBellPairs (CTag output) (STag output), output)
    toList = unListOutput
    fromList = ListOutput

-- TODO: fix undecideable instance caused by Ord (RTag) 
instance (Output output, Tag (STag output), Default (STag output), Tag (RTag output), Tag (CTag output), Semigroup (CTag output))
        => Output (ListOutput output) where
    type STag (ListOutput output) = (STag output)
    type RTag (ListOutput output) = (RTag output)
    type CTag (ListOutput output) = (CTag output)
    type OutputM (ListOutput output) = OutputM output
    computeOutput (ListOutput xs) = computeOutputHelper xs
      where
        computeOutputHelper [] untouched = cpure untouched
        computeOutputHelper ((i, o):ios) bps = 
            mconcat
                [ cjoin $ cmap (\x -> cmap (x <>) tl) (computeOutput o (chosen partial)) 
                | partial <- findElemsNDT staticBellPair (toList (Mset.bellPairs i)) bps
                , let tl = computeOutputHelper ios (rest partial)
                ]    

instance (OpOutput output op, Tag (STag output), Default (STag output), Monoid (CTag output), Tag (RTag output), Tag (CTag output))
         => OpOutput (ListOutput output) op where
        fromCBPOutput i@(Mset.LMS (ms, ())) o p =
            fromList [(Mset.LMS (ms, mempty), fromCBPOutput i o p)]

instance (DDom tag, Default tag, Semigroup cTag, DDom cTag) 
    => Output (D' (LabelledBellPairs cTag tag)) where
    type STag (D' (LabelledBellPairs cTag tag)) = tag
    type RTag (D' (LabelledBellPairs cTag tag)) = tag
    type CTag (D' (LabelledBellPairs cTag tag)) = cTag
    type OutputM (D' (LabelledBellPairs cTag tag)) = CD'
    computeOutput x _ = fromList [x]

instance (DDom tag, Default tag) => OpOutput (D' (LabelledBellPairs () tag)) Probability where
    fromCBPOutput _ o p
      | p == 1 = cpure (Mset.singleton' o)
      | p == 0 = cpure mempty
      | otherwise = D.choose p (Mset.singleton' o) mempty

