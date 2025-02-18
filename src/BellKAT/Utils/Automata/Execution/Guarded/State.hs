{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE StandaloneDeriving #-}
module BellKAT.Utils.Automata.Execution.Guarded.State
    ( StateSystem(..)
    , executeGuarded
    ) where

-- TODO: usually we abstract these away
import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Default
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Functor.Classes (Eq1(..), eq1, Show1(..), showsPrec1)
import           Data.List (intercalate)

import BellKAT.Definitions.Structures.Basic
import BellKAT.Utils.Automata.Transitions hiding (State)
import BellKAT.Utils.Automata.Transitions.Guarded
import BellKAT.Utils.Automata.Execution.Guarded.Internal
import BellKAT.Utils.Automata.Guarded

type StateTransitionSystem k s = IntMap (Map s (k (Int, s)))

data StateSystem k s = SS 
    { ssInitial :: (Int, s) 
    , ssTransitions :: StateTransitionSystem k s
    } 

instance (Show1 k, Show s, Eq s) => Show (StateSystem k s) where
    show x = intercalate "\n" $
        map showState $ IM.toList (ssTransitions x)
      where
        showState (s, sTr) = 
            (if s == fst (ssInitial x) then "^" else "") 
            <> show s <> ":\n  "
            <> intercalate "\n  " (map (showStateTr s) . Map.toList $ sTr)
        showStateTr i (s, sTr) =
            (if (i, s) == ssInitial x then "^" else "") 
            <> show s <> ": " <> showsPrec1 0 sTr ""

instance (Eq1 k, Eq s) => Eq (StateSystem k s) where
    (SS i t) == (SS i' t') = i == i' && liftEq (liftEq eq1) t t'

type ExecutionState k s = StateTransitionSystem k s

-- TODO: code can be shared with Execution.Guarded
executeGuarded 
    :: (Boolean t, Ord s, MonadPlus k, Foldable k) -- TODO: MonadPlus vs Monad + Monoid?
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
    :: (Boolean t, Ord s, MonadPlus k, Foldable k) 
    => Int -> s -> ExecutionMonad k t a s ()
computeFrom i st = getOrCompute i st >>= mapM_ (uncurry getOrCompute)

getOrCompute 
    :: (Boolean t, Ord s, MonadPlus k, Foldable k) 
    => Int -> s -> ExecutionMonad k t a s (k (Int, s))
getOrCompute i st =
    gets (Map.lookup st . (IM.! i)) >>= \case
        Just r -> pure r
        Nothing -> do
            r <- compute i st
            modify' (IM.update (Just . Map.insert st r) i)
            pure r

compute 
    :: (Boolean t, Ord s, MonadPlus k, Foldable k,
        MonadReader (ExecutionEnvironment k t a s) m) 
    => Int -> s -> m (k (Int, s))
compute i st = 
    computeTransitionsAtState i st >>= \case
      Nothing -> pure mzero 
      Just Done -> pure mzero -- TODO: should be smthing like mone
      Just (Step f j) -> pure $ (j,) <$> f
