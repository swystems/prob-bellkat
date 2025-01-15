{-# LANGUAGE StrictData #-}
module BellKAT.Implementations.Automata.EpsNFA 
    ( EpsNFA (..)
    , Eps (..)
    ) where

import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IS
import           Data.These

import BellKAT.Definitions.Structures.Basic
import BellKAT.Implementations.Automata.Internal

data Eps = Eps deriving stock (Show)

data EpsNFA a = ENFA
    { enfaInitial    :: Int
    , enfaTransition :: !(IntMap (IntMap (These Eps a)))
    , enfaFinal      :: !IntSet
    }

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

instance Show a => Show (EpsNFA a) where
    show x = unlines $
        map showState $ IM.toList (enfaTransition x)
      where
        showState (s, sTr) = 
            (if s == enfaInitial x then "^" else "") 
            <> show s 
            <> (if IS.member s (enfaFinal x) then "$" else "")
            <> ": "
            <> unwords (map showEpsTransition $ IM.toList sTr)

showEpsTransition :: Show a => (Int, These Eps a) -> String
showEpsTransition (j, This Eps) = "-()-> " <> show j
showEpsTransition (j, These Eps act) = "-( eps | " <> show act <> " )-> " <> show j
showEpsTransition (j, That x) = showTransition (j, x)


instance (ChoiceSemigroup a) => Semigroup (EpsNFA a) where
    (ENFA aI aT aF) <> (ENFA bI bT bF) =
        let
            nbI = bI + IM.size aT
            nbT = shiftTransitionUp (IM.size aT) bT
            nF = shiftFinalUp (IM.size aT) bF
            nabT = IM.fromSet (const $ IM.singleton nbI $ This Eps) aF
            nT = nabT `unionTransition` aT `unionTransition` nbT
         in ENFA aI nT nF

instance ChoiceSemigroup a => ChoiceSemigroup (EpsNFA a) where
    (ENFA aI aT aF) <+> (ENFA bI bT bF) =
        let
            naI = aI + 1
            naF = shiftFinalUp 1 aF
            naT = shiftTransitionUp 1 aT
            nbI = bI + 1 + IM.size aT
            nbT = shiftTransitionUp (1 + IM.size aT) bT
            nbF = shiftFinalUp (1 + IM.size aT) bF
            nabT = IM.singleton 0 $ IM.singleton naI (This Eps) <> IM.singleton nbI (This Eps)
         in ENFA 0 (naT `unionTransition` nbT `unionTransition` nabT) (naF <> nbF)
