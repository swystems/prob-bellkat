module BellKAT.Utils.Automata.Eps 
    ( Eps(..)
    , showEpsTransition
    ) where

import           Data.These

import BellKAT.Definitions.Structures.Basic
import BellKAT.Utils.Automata.Transitions

data Eps = Eps deriving stock (Show, Eq)

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

showEpsTransition :: Show a => (Int, These Eps a) -> String
showEpsTransition (j, This Eps) = "-()-> " <> show j
showEpsTransition (j, These Eps act) = "-( eps | " <> show act <> " )-> " <> show j
showEpsTransition (j, That x) = showTransition (j, x)
