{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ConstraintKinds #-}
module BellKAT.Utils.Automata.Execution.Guarded.State
    ( StateSystem(..)
    , CanExecuteState
    , executeGuarded
    ) where

import qualified Data.IntMap.Strict           as IM
import qualified Data.Map.Strict              as Map
import           Data.Default
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Subcategory.Functor
import           Control.Subcategory.Bind

import BellKAT.Definitions.Structures.Basic
import BellKAT.Utils.Automata.Transitions hiding (State)
import BellKAT.Utils.Automata.Transitions.Guarded
import BellKAT.Utils.Automata.Transitions.Functorial
import BellKAT.Utils.Automata.Execution.Guarded.Internal
import BellKAT.Utils.Automata.Guarded

type ExecutionState k s = StateTransitionSystem k s

type CanExecuteState t k s = (Boolean t, Ord s, Dom k s, Dom k (Int, s), CBind k, Monoid (k (Int, s)), Foldable k)

-- TODO: code can be shared with Execution.Guarded
executeGuarded 
    :: (CanExecuteState t k s)
    => (t -> s -> Bool)
    -> (a -> s -> k s) -- | executing one action
    -> GuardedFA t a
    -> s -> StateSystem k s
executeGuarded executeTest executeStep gfa x = 
    let ts = execExecution executeTest executeStep gfa $ computeFrom (initialState gfa) x
    in SS { ssInitial = (initialState gfa, x), ssTransitions = ts }

initialExecutionState :: GuardedFA t a -> ExecutionState k s
initialExecutionState gfa = IM.fromSet (const Map.empty) (states $ gfaTransition gfa)

type ExecutionMonad k t a s = ReaderT (ExecutionEnvironment k t a s) (State (ExecutionState k s))

execExecution 
    :: (t -> s -> Bool)
    -> (a -> s -> k s)
    -> GuardedFA t a
    -> ExecutionMonad k t a s b
    -> ExecutionState k s
execExecution executeTest executeStep gfa m =
    let env = EE { eeAutomaton = gfa, eeTestEvaluation = executeTest, eeStepEvaluation = executeStep, eeExecutionParams = def }
        st = initialExecutionState gfa
     in (`execState` st) . (`runReaderT` env) $ m

computeFrom 
    :: CanExecuteState t k s 
    => Int -> s -> ExecutionMonad k t a s ()
computeFrom i st = getOrCompute i st >>= mapM_ (uncurry computeFrom)

getOrCompute 
    :: (Boolean t, Ord s, Dom k s, Dom k (Int, s), CBind k, Monoid (k (Int, s)), Foldable k) 
    => Int -> s -> ExecutionMonad k t a s (k (Int, s))
getOrCompute i st =
    gets (Map.lookup st . (IM.! i)) >>= \case
        Just r -> pure r
        Nothing -> do
            r <- compute i st
            modify' (IM.update (Just . Map.insert st r) i)
            pure r

compute 
    :: (CanExecuteState t k s, MonadReader (ExecutionEnvironment k t a s) m) 
    => Int -> s -> m (k (Int, s))
compute i st = 
    computeTransitionsAtState i st >>= \case
      Nothing -> pure mempty 
      Just Done -> pure mempty -- TODO: should be smthing like mone
      Just (Step f j) -> pure $ cmap (j,) f
