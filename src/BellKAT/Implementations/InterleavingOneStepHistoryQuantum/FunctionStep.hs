{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE DataKinds      #-}

module BellKAT.Implementations.InterleavingOneStepHistoryQuantum.FunctionStep
    ( FunctionStep (..)
    , PartialNDEndo (..)
    ) where

import           Data.Kind
import           Data.Functor.Classes

import qualified BellKAT.Utils.Multiset                as Mset
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Tests
import           BellKAT.Definitions.Structures
import           BellKAT.Utils.Choice
import           BellKAT.Utils.PartialNDEndo

-- | `FunctionStep` represents one step of the execution as: H -> M(H x H) (aka `PartialNDEndo`)
newtype FunctionStep (test :: Type -> Type) tag = FunctionStep
    { executeFunctionStep :: PartialNDEndo (History tag)
    } deriving newtype (Semigroup)

instance Show1 (FunctionStep test) where
  liftShowsPrec _ _ _ _ = shows "FunctionStep [\\h -> ..]"

instance Ord tag => ChoiceSemigroup (FunctionStep test tag) where
    (FunctionStep p) <+> (FunctionStep q) = FunctionStep . PartialNDEndo $
        \h -> applyPartialNDEndo p h <> applyPartialNDEndo q h

instance (Ord tag) => CreatesBellPairs (FunctionStep test tag) tag where
    tryCreateBellPairFrom (CreateBellPairArgs bps bp prob dk) =
        FunctionStep . PartialNDEndo $ \h@(History ts) ->
            case findTreeRootsND bps ts of
            [] -> [chooseNoneOf h]
            partialNewTs ->
                mconcat
                [ case prob of
                    1.0 -> [ History <$> mapChosen (Mset.singleton . processDup dk bp) partial ]
                    _  -> [ History <$> mapChosen (Mset.singleton . processDup dk bp) partial
                                , History <$> partial { chosen = [] }
                                ]
                | partial <- partialNewTs
                ]

instance (Ord tag, Test test) => Tests (FunctionStep test tag) test tag where
  test t = FunctionStep . PartialNDEndo $ \h@(History ts) ->
    if getBPsPredicate (toBPsPredicate t) (Mset.map rootLabel ts) then [ chooseNoneOf h ] else []
