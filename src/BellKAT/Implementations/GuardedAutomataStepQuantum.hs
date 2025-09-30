module BellKAT.Implementations.GuardedAutomataStepQuantum 
    ( GuardedAutomatonStepQuantum (getGFA)
    , execute
    , executeWith
    , executeSystem
    , executeState
    , StateSystem
    , ComputedState
    ) where

import           Data.Pointed
import           Data.Maybe                     (fromJust)
import           Data.Default (def)

import           BellKAT.Definitions.Structures
import           BellKAT.Utils.Automata.Guarded
import           BellKAT.Utils.Automata.Transitions.Functorial (ComputedState)
import qualified BellKAT.Utils.Automata.Execution.Guarded as GAE
import           BellKAT.Utils.Automata.Execution.Guarded (ExecutionParams, CanExecuteGuarded)
import           BellKAT.Utils.Automata.Execution.Guarded.State

-- | A wrapper around `GuardedFA` parameterized by tests `t` and actions `a`.
--
-- Implements a bunch of classes from v`BellKAT.Definitions.Structures.Basic` and v`BellKAT.Definitions.Structures.Quantum` turning `GuardedFA` into an
-- appropriate target for interpreting guarded policies.
newtype GuardedAutomatonStepQuantum t a = GASQ 
    { getGFA :: GuardedFA t a
    } deriving newtype (Show, Semigroup, Monoid, ParallelSemigroup, OrderedSemigroup, Pointed)

instance (Show a, Show t, DecidableBoolean t) => Guarded t (GuardedAutomatonStepQuantum t a) where
    ite t (GASQ a) (GASQ b) = GASQ $ ite t a b

instance (Show t, DecidableBoolean t, CreatesBellPairs (sq tag) op tag)
  => CreatesBellPairs (GuardedAutomatonStepQuantum t (sq tag)) op tag where
    tryCreateBellPairFrom = point . tryCreateBellPairFrom

instance (Show t, Show (sq tag), DecidableBoolean t, ParallelSemigroup (sq tag), CreatesBellPairs (sq tag) op tag) 
  => Quantum (GuardedAutomatonStepQuantum t (sq tag)) op tag where

-- | "executes" `GuardedAutomatonStepQuantum` using respective interpretation functions
executeWith :: CanExecuteGuarded t k b
    => ExecutionParams b
    -> (t -> b -> Bool) 
    -- ^ Interpretation for tests as predicate over state
    -> (a -> b -> k b)
    -- ^ Interpretation for actions as monadic transformations of states
    -> GuardedAutomatonStepQuantum t a
    -> b 
    -- ^ Initial state
    -> Maybe (k b)
executeWith params executeTest executeStep (GASQ nfa) = 
    GAE.execute params executeTest executeStep nfa
--TODO: that's a weird one, maybe we should operate with GuardedFA directly

execute :: CanExecuteGuarded t k b
    => (t -> b -> Bool)
    -> (a -> b -> k b)
    -> GuardedAutomatonStepQuantum t a
    -> b -> k b
execute executeTest executeStep gasq = fromJust . executeWith def executeTest executeStep gasq

executeSystem :: CanExecuteState t k b
    => (t -> b -> Bool)
    -> (a -> b -> k b)
    -> GuardedAutomatonStepQuantum t a
    -> b -> StateSystem k b
executeSystem executeTest executeStep (GASQ gasq) = executeGuarded executeTest executeStep gasq

executeState :: CanExecuteGuarded t k b
    => (t -> b -> Bool)
    -> (a -> b -> k b)
    -> GuardedAutomatonStepQuantum t a
    -> b -> ComputedState k b
executeState executeTest executeStep (GASQ gasq) = fromJust . GAE.executeAll def executeTest executeStep gasq
