{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ConstraintKinds #-}
module BellKAT.Utils.Automata.Execution.Guarded 
    ( execute
    , executeAll
    , ExecutionParams(..)
    , CanExecuteGuarded
    ) where

import           Data.Foldable
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           GHC.Exts (fromList)
import           Control.Subcategory.Functor
import           Control.Subcategory.Bind

import BellKAT.Definitions.Structures.Basic
import BellKAT.Utils.Automata.Transitions hiding (State)
import BellKAT.Utils.Automata.Transitions.Guarded
import BellKAT.Utils.Automata.Transitions.Functorial
import BellKAT.Utils.Automata.Execution.Common
import BellKAT.Utils.Automata.Guarded

import BellKAT.Utils.Automata.Execution.Guarded.Internal

type CanExecuteGuarded t k s = (Boolean t, Ord s, Dom k s, CMonad k, Monoid (k s), Foldable k)

execute
    :: CanExecuteGuarded t k s
    => ExecutionParams s
    -> (t -> s -> Bool)
    -> (a -> s -> k s) -- | executing one action
    -> GuardedFA t a
    -> s -> Maybe (k s)
execute params executeTest executeStep gfa x =
    let err = evalExecution params executeTest executeStep gfa
            $ getOrCompute (initialState gfa) x
    in case err of 
         Left _ -> Nothing
         Right r -> Just r

executeAll
    :: CanExecuteGuarded t k s
    => ExecutionParams s
    -> (t -> s -> Bool)
    -> (a -> s -> k s) -- | executing one action
    -> GuardedFA t a
    -> s -> Maybe (ComputedState k s)
executeAll params executeTest executeStep gfa x =
    let err = execExecution params executeTest executeStep gfa
            $ getOrCompute (initialState gfa) x
    in case err of 
         Left _ -> Nothing
         Right r -> Just r

type ExecutionState k s = ComputedState k s

type ExecutionMonad k t a s = ReaderT (ExecutionEnvironment k t a s) (StateT (ExecutionState k s) (Except ExecutionError))

evalExecution 
    :: ExecutionParams s
    -> (t -> s -> Bool)
    -> (a -> s -> k s)
    -> GuardedFA t a
    -> ExecutionMonad k t a s b
    -> Either ExecutionError b
evalExecution params executeTest executeStep gfa m = 
    let env = EE { eeAutomaton = gfa, eeTestEvaluation = executeTest, eeStepEvaluation = executeStep, eeExecutionParams = params }
        st = initialExecutionState gfa
     in  runExcept . (`evalStateT` st) . (`runReaderT` env) $ m

execExecution
    :: ExecutionParams s
    -> (t -> s -> Bool)
    -> (a -> s -> k s)
    -> GuardedFA t a
    -> ExecutionMonad k t a s b
    -> Either ExecutionError (ExecutionState k s)
execExecution params executeTest executeStep gfa m = 
    let env = EE { eeAutomaton = gfa, eeTestEvaluation = executeTest, eeStepEvaluation = executeStep, eeExecutionParams = params }
        st = initialExecutionState gfa
     in runExcept . (`execStateT` st) . (`runReaderT` env) $ m

initialExecutionState :: GuardedFA t a -> ExecutionState m s
initialExecutionState gfa = fromList [(x, Map.empty) | x <- statesToList $ states $ gfaTransition gfa ]

getOrComputeAll
    :: CanExecuteGuarded t k s => Int -> k s -> ExecutionMonad k t a s (Map s (k s))
getOrComputeAll i fs =
    forM_ fs (getOrCompute i) >> gets (! i)

getOrCompute :: CanExecuteGuarded t k s => Int -> s -> ExecutionMonad k t a s (k s)
getOrCompute i st =
    gets (Map.lookup st . (! i)) >>= \case
        Just r -> pure r
        Nothing -> do
            checkStateLimit i
            r <- compute i st
            modify' (insertWith (<>) i [(st, r)])
            pure r

compute :: CanExecuteGuarded t k s => Int -> s -> ExecutionMonad k t a s (k s)
compute i st =
    computeTransitionsAtState i st >>= \case
      Nothing -> pure mempty 
      Just Done -> pure $ creturn st
      Just (Step f j) -> do
        allTransitions <- getOrComputeAll j f
        pure $ f >>- (allTransitions Map.!)

checkStateLimit :: Int -> ExecutionMonad k t a s ()
checkStateLimit i =
    reader (maxOptionsPerState . eeExecutionParams) >>= \case
        Nothing -> pure ()
        Just mo -> do
            curSize <- gets $ Map.size . (! i)
            when (curSize >= mo) $
                throwError TooManyStates
