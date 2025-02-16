{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module BellKAT.Utils.Automata.Execution.Guarded 
    ( execute
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

execute 
    :: (Boolean t, Ord s, MonadPlus k, Foldable k)
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

newtype ExecutionState m s = ES { unES :: IntMap (Map s (m s)) }

data ExecutionEnvironment m t a s = EE
    { eeAutomaton :: GuardedFA t a
    , eeTestEvaluation :: t -> s -> Bool
    , eeStepEvaluation :: a -> s -> m s
    , eeExecutionParams :: ExecutionParams s
    }

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
     in (`evalState` initialExecutionState gfa) . runExceptT . (`runReaderT` env) $ m

initialExecutionState :: GuardedFA t a -> ExecutionState m s
initialExecutionState gfa = ES $ IM.fromSet (const Map.empty) (states $ gfaTransition gfa)

getOrComputeAll
    :: (Foldable k, Boolean t, Ord s, MonadPlus k)
    => Int -> k s -> ExecutionMonad k t a s (Map s (k s))
getOrComputeAll i fs =
    forM_ fs (getOrCompute i) >> gets ((IM.! i) . unES)


getOrCompute 
    :: (Boolean t, Ord s, MonadPlus k, Foldable k) 
    => Int -> s -> ExecutionMonad k t a s (k s)
getOrCompute i st =
    gets (Map.lookup st . (IM.! i) . unES) >>= \case
        Just r -> pure r
        Nothing -> do
            checkStateLimit i
            r <- compute i st
            modify' (ES . IM.update (Just . Map.insert st r) i . unES)
            pure r

compute 
    :: (Boolean t, Ord s, MonadPlus k, Foldable k) 
    => Int -> s -> ExecutionMonad k t a s (k s)
compute i st = do
    ts <- reader ((! i) . transitionSystem . eeAutomaton) 
    tsAtSt <- evalTransitions 
        <$> reader eeTestEvaluation <*> reader eeStepEvaluation <*> pure ts <*> pure st
    case tsAtSt of
      Nothing -> pure mzero 
      Just Done -> pure $ pure st
      Just (Step f j) -> do
        allTransitions <- getOrComputeAll j f
        pure $ f >>= (allTransitions Map.!)

evalTransitions 
    :: (t -> s -> Bool)
    -> (a -> s -> k s)
    -> GuardedTransitions t a 
    -> s 
    -> Maybe (Next (k s))
evalTransitions executeTest executeStep ts x = 
    case findMatchingAction executeTest ts x of
        Nothing -> Nothing
        Just Done -> Just Done
        Just (Step a k) -> Just (Step (executeStep a x) k)

checkStateLimit :: Int -> ExecutionMonad k t a s ()
checkStateLimit i =
    reader (maxOptionsPerState . eeExecutionParams) >>= \case
        Nothing -> pure ()
        Just mo -> do
            curSize <- gets $ Map.size . (IM.! i) . unES
            when (curSize >= mo) $
                throwError TooManyStates

findMatchingAction 
    :: (t -> s -> Bool) -> GuardedTransitions t a -> s -> Maybe (Next a)
findMatchingAction evalTest ts x = 
    snd <$> find ((`evalTest` x) . fst) (gTransitionsToList ts)

