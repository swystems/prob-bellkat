{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module BellKAT.Utils.Automata.Execution.Guarded 
    ( execute
    , ExecutionParams(..)
    ) where

import           Data.Foldable
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map

import BellKAT.Definitions.Structures.Basic
import BellKAT.Utils.Automata.Transitions hiding (State)
import BellKAT.Utils.Automata.Transitions.Guarded
import BellKAT.Utils.Automata.Execution.Common
import BellKAT.Utils.Automata.Guarded

import BellKAT.Utils.Automata.Execution.Guarded.Internal

execute 
    :: (Boolean t, Ord s, MonadPlus k, Foldable k) -- TODO: MonadPlus vs Monad + Monoid?
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

type ExecutionState m s = IntMap (Map s (m s))

type ExecutionMonad k t a s = ReaderT (ExecutionEnvironment k t a s) (ExceptT ExecutionError (State (ExecutionState k s)))

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
     in (`evalState` st) . runExceptT . (`runReaderT` env) $ m

initialExecutionState :: GuardedFA t a -> ExecutionState m s
initialExecutionState gfa = IM.fromSet (const Map.empty) (states $ gfaTransition gfa)

getOrComputeAll
    :: (Foldable k, Boolean t, Ord s, MonadPlus k)
    => Int -> k s -> ExecutionMonad k t a s (Map s (k s))
getOrComputeAll i fs =
    forM_ fs (getOrCompute i) >> gets (IM.! i)

getOrCompute 
    :: (Boolean t, Ord s, MonadPlus k, Foldable k) 
    => Int -> s -> ExecutionMonad k t a s (k s)
getOrCompute i st =
    gets (Map.lookup st . (IM.! i)) >>= \case
        Just r -> pure r
        Nothing -> do
            checkStateLimit i
            r <- compute i st
            modify' (IM.update (Just . Map.insert st r) i)
            pure r

compute 
    :: (Boolean t, Ord s, MonadPlus k, Foldable k) 
    => Int -> s -> ExecutionMonad k t a s (k s)
compute i st =
    computeTransitionsAtState i st >>= \case
      Nothing -> pure mzero 
      Just Done -> pure $ pure st
      Just (Step f j) -> do
        allTransitions <- getOrComputeAll j f
        pure $ f >>= (allTransitions Map.!)

checkStateLimit :: Int -> ExecutionMonad k t a s ()
checkStateLimit i =
    reader (maxOptionsPerState . eeExecutionParams) >>= \case
        Nothing -> pure ()
        Just mo -> do
            curSize <- gets $ Map.size . (IM.! i)
            when (curSize >= mo) $
                throwError TooManyStates
