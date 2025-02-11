module BellKAT.Implementations.GuardedAutomataStepQuantum 
    ( GuardedAutomatonStepQuantum (getGFA)
    ) where

import           Data.Pointed

import           BellKAT.Definitions.Structures
import           BellKAT.Utils.Automata.Guarded

newtype GuardedAutomatonStepQuantum t a = GASQ 
    { getGFA :: GuardedFA t a
    } deriving newtype (Show, Semigroup, ParallelSemigroup, OrderedSemigroup, Pointed)

instance (Show t, DecidableBoolean t) => Guarded t (GuardedAutomatonStepQuantum t a) where
    ite t (GASQ a) (GASQ b) = GASQ $ ite t a b

instance (Show t, DecidableBoolean t, CreatesBellPairs (sq tag) tag)
  => CreatesBellPairs (GuardedAutomatonStepQuantum t (sq tag)) tag where
    tryCreateBellPairFrom = point . tryCreateBellPairFrom

instance (Show t, Show (sq tag), DecidableBoolean t, ParallelSemigroup (sq tag), CreatesBellPairs (sq tag) tag) 
  => Quantum (GuardedAutomatonStepQuantum t (sq tag)) tag where
