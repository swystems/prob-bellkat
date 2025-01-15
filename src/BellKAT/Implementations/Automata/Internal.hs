module BellKAT.Implementations.Automata.Internal where

import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IS
import           Data.These

import BellKAT.Definitions.Structures.Basic

data Eps = Eps deriving stock (Show)

instance (ChoiceSemigroup a) => ChoiceSemigroup (These Eps a) where
    This Eps <+> This Eps       = This Eps
    This Eps <+> That x         = These Eps x
    This Eps <+> These Eps x    = These Eps x
    That x <+> This Eps         = These Eps x
    That x <+> That y           = That (x <+> y)
    That x <+> These Eps y      = These Eps (x <+> y)
    These Eps x <+> These Eps y = These Eps (x <+> y)
    These Eps x <+> This Eps    = These Eps x
    These Eps x <+> That y      = These Eps (x <+> y)

showTransition :: Show a => (Int, a) -> String
showTransition (j , act) = "-( " <> show act <> " )-> " <> show j

class CanShiftUp a where
    shiftUp :: Int -> a -> a

instance CanShiftUp IntSet where
    shiftUp k = IS.mapMonotonic (+ k) 

instance CanShiftUp (IntMap (IntMap a)) where
    shiftUp k = IM.map (IM.mapKeysMonotonic (+ k)) . IM.mapKeysMonotonic (+k)

unionTransition
    :: ChoiceSemigroup a
    => IntMap (IntMap a) -> IntMap (IntMap a) -> IntMap (IntMap a)
unionTransition = IM.unionWith (IM.unionWith (<+>))

