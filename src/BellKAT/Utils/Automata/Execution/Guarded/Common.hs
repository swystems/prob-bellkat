module BellKAT.Utils.Automata.Execution.Guarded.Common
    ( StateSystem (..)
    , StateTransitionSystem
    , ComputedState
    ) where 

import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Functor.Classes (Eq1(..), eq1, Show1(..), showsPrec1)
import           Data.List (intercalate)

-- TODO: usually we abstract these away
type StateTransitionSystem k s = IntMap (Map s (k (Int, s)))

data StateSystem k s = SS 
    { ssInitial :: (Int, s) 
    , ssTransitions :: StateTransitionSystem k s
    } 

instance (Show1 k, Show s, Eq s) => Show (StateSystem k s) where
    show x = intercalate "\n" $
        map showState $ IM.toList (ssTransitions x)
      where
        showState (s, sTr) = 
            (if s == fst (ssInitial x) then "^" else "") 
            <> show s <> ":\n  "
            <> intercalate "\n  " (map (showStateTr s) . Map.toList $ sTr)
        showStateTr i (s, sTr) =
            (if (i, s) == ssInitial x then "^" else "") 
            <> show s <> ": " <> showsPrec1 0 sTr ""

instance (Eq1 k, Eq s) => Eq (StateSystem k s) where
    (SS i t) == (SS i' t') = i == i' && liftEq (liftEq eq1) t t'

type ComputedState k s = IntMap (Map s (k s))

