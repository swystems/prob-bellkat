{-# LANGUAGE StrictData #-}
module BellKAT.Utils.Automata.GuardedEps
    ( GuardedEpsFA (..)
    ) where

import BellKAT.Definitions.Structures.Basic
import BellKAT.Utils.Automata.Transitions
import BellKAT.Utils.Automata.Transitions.Guarded
import BellKAT.Utils.Automata.Eps

data GuardedEpsFA t a = GEFA
    { gfaInitial :: Int
    , gfaTransition :: GuardedTransitionSystem t (Either Eps a)
    }

instance (Show a, Show t, Boolean t) => Show (GuardedEpsFA t a) where
    show x = unlines $
        map showState $ toTransitionsList (gfaTransition x)
      where
        showState (s, sTr) = 
            (if s == gfaInitial x then "^" else "") 
            <> show s 
            <> ": "
            <> showGuardedTransitionsWith showGuardedEpsTransition sTr

showGuardedEpsTransition :: (Show t, Show a) => (t, Next (Either Eps a)) -> String
showGuardedEpsTransition (t, Step (Left Eps) j) = "[" <> show t <> "]" <> "-()-> " <> show j
showGuardedEpsTransition (t, Step (Right act) a) = showGuardedTransition (t, Step act a)
showGuardedEpsTransition (t, n) = showGuardedTransition (t, n)

instance DecidableBoolean t => Semigroup (GuardedEpsFA t a) where
    (GEFA aI aT) <> (GEFA bI bT) =
        let
            nbI = bI + numStates aT
            naT = setDoneToStep (Left Eps) nbI aT
            nbT = shiftUp (numStates aT) bT
            nT = naT <> nbT
         in GEFA aI nT

instance DecidableBoolean t => Guarded t (GuardedEpsFA t a) where
  ite t (GEFA aI aT) (GEFA bI bT) = 
      let 
        naI = aI + 1
        naT = shiftUp 1 aT
        nbI = bI + 1 + numStates aT
        nbT = shiftUp (1 + numStates aT) bT
        nabT = singletonGts 0 t (Left Eps) naI
            <> singletonGts 0 (notB t) (Left Eps) nbI
       in GEFA 0 (naT <> nbT <> nabT)
