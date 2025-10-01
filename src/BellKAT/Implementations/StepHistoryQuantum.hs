{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeApplications #-}
module BellKAT.Implementations.StepHistoryQuantum (execute) where

import           Data.List              (foldl')
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Pointed

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Structures

-- ** Quantum operations represented as a sequence of primitive actions
newtype StepHistoryQuantum a = StepHistoryQuantum
    { getSteps :: [[a]]
    }

instance Pointed StepHistoryQuantum where
    point x = StepHistoryQuantum [[x]]

instance ParallelSemigroup a =>  ParallelSemigroup (StepHistoryQuantum a) where
    shq <||> shq' = StepHistoryQuantum $ [ steps <|||> steps' | steps <- getSteps shq, steps' <- getSteps shq']

instance Semigroup (StepHistoryQuantum a) where
    shq <> shq' = StepHistoryQuantum $ (<>) <$> getSteps shq <*> getSteps shq'

instance Monoid (StepHistoryQuantum a) where
    mempty = StepHistoryQuantum [[]]

(<|||>) :: ParallelSemigroup a => [a] -> [a] -> [a]
steps <|||> steps' =
    let shortestLength = minimum @[] $ length <$> [steps, steps']
        commonSteps = take shortestLength
        restSteps = drop shortestLength
    in [hq <||> hq' | hq <- commonSteps steps | hq' <- commonSteps steps']
            ++ restSteps steps ++ restSteps steps'

instance (Ord t, CreatesBellPairs (sq t) op t) => CreatesBellPairs (StepHistoryQuantum (sq t)) op t where
    tryCreateBellPairFrom = point . tryCreateBellPairFrom

instance (Ord t, Quantum (sq t) op t) => Quantum (StepHistoryQuantum (sq t)) op t where

instance Layered (StepHistoryQuantum (sq t)) where
    newtype Layer (StepHistoryQuantum (sq t)) = OneStep (sq t)
    liftLayer (OneStep s) = point s

instance (Ord t, Quantum (sq t) op t) => OrderedLayeredQuantum (StepHistoryQuantum (sq t)) op t where
    orderedTryCreateBellPairFrom = OneStep . tryCreateBellPairFrom

instance (Semigroup (sq t)) => OrderedSemigroup (Layer (StepHistoryQuantum (sq t))) where
   (OneStep s) <.> (OneStep s') = OneStep (s <> s')

instance ChoiceSemigroup (StepHistoryQuantum a) where
    hq <+> hq' = StepHistoryQuantum $ getSteps hq <> getSteps hq'

instance (Ord t, Tests (sq t) test t) => TestsOrderedLayered (StepHistoryQuantum (sq t)) test t where
    orderedTest = OneStep . test

instance (Ord t, Tests (sq t) test t, Quantum (sq t) op t) 
    => TestsOrderedLayeredQuantum (StepHistoryQuantum (sq t)) test op t where

execute :: Ord t
    => (a -> History t -> Set (History t))
    -> StepHistoryQuantum a
    -> History t -> Set (History t)
execute executeStep shq h = mconcat
    [ foldl' (\hs hq -> Set.unions (Set.map (executeStep hq) hs)) (Set.singleton h) steps
    | steps <- getSteps shq
    ]
