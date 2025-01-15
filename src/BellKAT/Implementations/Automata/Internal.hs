module BellKAT.Implementations.Automata.Internal where

import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IS

import BellKAT.Definitions.Structures.Basic

showTransition :: Show a => (Int, a) -> String
showTransition (j , act) = "-( " <> show act <> " )-> " <> show j

shiftFinalUp :: Int -> IntSet -> IntSet
shiftFinalUp k = IS.mapMonotonic (+ k)

shiftTransitionUp :: Int -> IntMap (IntMap a) -> IntMap (IntMap a)
shiftTransitionUp k = IM.map (IM.mapKeysMonotonic (+ k)) . IM.mapKeysMonotonic (+k)

unionTransition
    :: ChoiceSemigroup a
    => IntMap (IntMap a) -> IntMap (IntMap a) -> IntMap (IntMap a)
unionTransition = IM.unionWith (IM.unionWith (<+>))
