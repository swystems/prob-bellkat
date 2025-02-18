{-# LANGUAGE StrictData #-}
module BellKAT.Utils.Automata.GuardedEps
    ( GuardedEpsFA (..)
    ) where

import Data.Pointed

import BellKAT.Definitions.Structures.Basic
import BellKAT.Utils.Automata.Transitions
import BellKAT.Utils.Automata.Transitions.Guarded
import BellKAT.Utils.Automata.Eps

data GuardedEpsFA t a = GEFA
    { gefaInitial :: Int
    , gefaTransition :: GuardedTransitionSystem t (Either Eps a)
    } deriving stock (Eq)

instance HasStates (GuardedEpsFA t a) where
  states = states . gefaTransition
  numStates = numStates . gefaTransition

instance CanMapStates (GuardedEpsFA t a) where
    mapStates f (GEFA i t) = GEFA (f i) (mapStates f t)
    mapStatesMonotonic f (GEFA i t) = GEFA (f i) (mapStatesMonotonic f t)

instance CanRestrictStates (GuardedEpsFA t a) where
  restrictStates s (GEFA i t) = 
      if not (i `statesMember` s)
         then error "Cannot restricted outside initial state"
         else GEFA i (restrictStates s t)

instance Boolean t => LikeAutomaton (GuardedEpsFA t) where
    type AutomatonAction (GuardedEpsFA t) a = Either Eps a
    type AutomatonTS (GuardedEpsFA t) = GuardedTransitionSystem t
    initialState = gefaInitial
    transitionSystem = gefaTransition

instance (Show a, Show t, DecidableBoolean t) => Show (GuardedEpsFA t a) where
    show x = unlines $
        map showState $ toPairs (gefaTransition x)
      where
        showState (s, sTr) = 
            (if s == gefaInitial x then "^" else "") 
            <> show s 
            <> ": "
            <> showGuardedTransitionsWith showGuardedEpsTransition sTr

showGuardedEpsTransition :: (Show t, Show a) => (t, Next (Either Eps a)) -> String
showGuardedEpsTransition (t, Step (Left Eps) j) = "[" <> show t <> "]" <> "-()-> " <> show j
showGuardedEpsTransition (t, Step (Right act) a) = showGuardedTransition (t, Step act a)
showGuardedEpsTransition (t, n) = showGuardedTransition (t, n)

instance (Show t, DecidableBoolean t) => Semigroup (GuardedEpsFA t a) where
    (GEFA aI aT) <> (GEFA bI bT) =
        let
            nbI = bI + numStates aT
            naT = setDoneToStep (Left Eps) nbI aT
            nbT = shiftUp (numStates aT) bT
            nT = naT <> nbT
         in removeUnreachable $ GEFA aI nT

instance (Show t, DecidableBoolean t) => Guarded t (GuardedEpsFA t a) where
  ite t (GEFA aI aT) (GEFA bI bT) = 
      let 
        naI = aI + 1
        naT = shiftUp 1 aT
        nbI = bI + 1 + numStates aT
        nbT = shiftUp (1 + numStates aT) bT
        nabT = singletonGts 0 t (Left Eps) naI
            <> singletonGts 0 (notB t) (Left Eps) nbI
       in GEFA 0 (naT <> nbT <> nabT)

instance (Show t, DecidableBoolean t) => Pointed (GuardedEpsFA t) where
    point x = GEFA 0 (gtsFromList 
        [ (0, [(true, Step (Right x) 1)])
        , (1, [(true, Done)])])
