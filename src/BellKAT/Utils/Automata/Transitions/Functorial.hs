{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Utils.Automata.Transitions.Functorial
    ( StateSystem (..)
    , StateTransitionSystem
    , ComputedState
    ) where 

import           Prelude hiding (lookup)
import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM hiding (fromList)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.List (intercalate)
import qualified GHC.Exts (IsList, Item)
import           GHC.Exts (fromList, toList)

import BellKAT.Utils.Automata.Transitions.Core

-- TODO: usually we abstract these away
type StateTransitionSystem k s = IntMap (Map s (k (Int, s)))

data StateSystem k s = SS 
    { ssInitial :: (Int, s) 
    , ssTransitions :: StateTransitionSystem k s
    } 

instance (Show (k (Int, s)), Show s, Eq s) => Show (StateSystem k s) where
    show x = intercalate "\n" $
        map showState $ IM.toList (ssTransitions x)
      where
        showState (s, sTr) = 
            (if s == fst (ssInitial x) then "^" else "") 
            <> show s <> ":\n  "
            <> intercalate "\n  " (map (showStateTr s) . Map.toList $ sTr)
        showStateTr i (s, sTr) =
            (if (i, s) == ssInitial x then "^" else "") 
            <> show s <> ": " <> show sTr

instance (Eq (k (Int, s)), Eq s) => Eq (StateSystem k s) where
    (SS i t) == (SS i' t') = i == i' && t == t'

newtype ComputedState k s = CS { unCS :: IntMap (Map s (k s)) }

instance StaticMap (ComputedState k s) where
    type Key (ComputedState k s) = State
    type Val (ComputedState k s) = Map s (k s) 
    size = size . unCS
    s `lookup` (CS xs) = s `lookup` xs
    s `member` (CS xs) = s `member` xs

deriving newtype instance DynamicMap (ComputedState k s)

instance GHC.Exts.IsList (ComputedState k s) where
    type Item (ComputedState k s) = (State, Map s (k s))
    fromList = CS . fromList
    toList = toList . unCS

instance (GHC.Exts.IsList (k s), Show (k s), Show s, Eq s) => Show (ComputedState k s) where
    show x = intercalate "\n" $
        map showState $ toList x
      where
        showState (s, sTr) = 
            show s <> ":\n  " <> intercalate "\n  " (map (showStateTr s) . Map.toList $ sTr)
        showStateTr _ (s, sTr) =
            show s <> ": " <> "(" <> show (length . toList $ sTr) <> ") " <> show sTr
