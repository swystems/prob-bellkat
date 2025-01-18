{-# LANGUAGE StrictData #-}
module BellKAT.Implementations.Automata.EpsNFA 
    ( EpsNFA (..)
    , Eps (..)
    ) where

import           Data.Pointed
import           Data.These

import BellKAT.Definitions.Structures.Basic
import BellKAT.Implementations.Automata.Transitions
import BellKAT.Implementations.Automata.Eps

data EpsNFA a = ENFA
    { enfaInitial    :: Int
    , enfaTransition :: TransitionSystem (These Eps a)
    , enfaFinal      :: States
    } deriving stock Eq

instance Show a => Show (EpsNFA a) where
    show x = unlines $
        map showState $ toTransitionsList (enfaTransition x)
      where
        showState (s, sTr) = 
            (if s == enfaInitial x then "^" else "") 
            <> show s 
            <> (if statesMember s (enfaFinal x) then "$" else "")
            <> ":\n"
            <> showTransitionsWith showEpsTransition sTr

instance (ChoiceSemigroup a) => Semigroup (EpsNFA a) where
    (ENFA aI aT aF) <> (ENFA bI bT bF) =
        let
            nbI = bI + numStates aT
            nbT = shiftUp (numStates aT) bT
            nF = shiftUp (numStates aT) bF
            nabT = sendStatesInto (This Eps) nbI aF
            nT = nabT <> aT <> nbT
         in ENFA aI nT nF

instance ChoiceSemigroup a => ChoiceSemigroup (EpsNFA a) where
    (ENFA aI aT aF) <+> (ENFA bI bT bF) =
        let
            naI = aI + 1
            naF = shiftUp 1 aF
            naT = shiftUp 1 aT
            nbI = bI + 1 + numStates aT
            nbT = shiftUp (1 + numStates aT) bT
            nbF = shiftUp (1 + numStates aT) bF
            nabT = tsFromList [(0, [(This Eps, naI), (This Eps, nbI)])]
         in ENFA 0 (naT <> nbT <> nabT) (naF <> nbF)

instance Pointed EpsNFA where
    point x = ENFA 0 (singletonTs 0 (That x) 1) (singletonState 1)
