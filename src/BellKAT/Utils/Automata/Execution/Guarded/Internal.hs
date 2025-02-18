module BellKAT.Utils.Automata.Execution.Guarded.Internal where

import           Data.Foldable
import           Control.Monad.Reader
import           Control.Subcategory.Bind

import BellKAT.Definitions.Structures.Basic
import BellKAT.Utils.Automata.Transitions hiding (State)
import BellKAT.Utils.Automata.Transitions.Guarded
import BellKAT.Utils.Automata.Guarded
import BellKAT.Utils.Automata.Execution.Common

findMatchingAction 
    :: (t -> s -> Bool) -> GuardedTransitions t a -> s -> Maybe (Next a)
findMatchingAction evalTest ts x = 
    snd <$> find ((`evalTest` x) . fst) (gTransitionsToList ts)

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

data ExecutionEnvironment k t a s = EE
    { eeAutomaton :: GuardedFA t a
    , eeTestEvaluation :: t -> s -> Bool
    , eeStepEvaluation :: a -> s -> k s
    , eeExecutionParams :: ExecutionParams s
    }

computeTransitionsAtState 
    :: (Boolean t, Ord s, CBind k, Foldable k, MonadReader (ExecutionEnvironment k t a s) m)
    => Int -> s -> m (Maybe (Next (k s)))
computeTransitionsAtState i st = do
    ts <- reader ((! i) . transitionSystem . eeAutomaton) 
    evalTransitions 
        <$> reader eeTestEvaluation <*> reader eeStepEvaluation <*> pure ts <*> pure st
