{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Implementations.AutomataStepQuantum 
    ( AutomatonStepQuantum (getNFA)
    , AutomatonChoice(..)
    , executeE
    , executeWithE
    , executeWith
    , execute
    , ExecutionParams(..)
    ) where

import           Data.Pointed
import           Data.List.NonEmpty             (NonEmpty)
import           Data.Set                       (Set)
import           Data.Maybe                     (fromJust)
import           Data.Default
 

import           BellKAT.Definitions.Structures
import           BellKAT.Utils.Automata
import qualified BellKAT.Utils.Automata.Execution.Set as AE
import           BellKAT.Utils.Automata.Execution.Set (ExecutionParams)
import           BellKAT.Utils.NonEmpty

-- | `AutomatonChoice` defines how non-deterministic transitions are handled by the automaton
data AutomatonChoice 
    -- | `ACNormal` has non-determinism represented as parallel transitions
    = ACNormal 
    -- | `ACEmbedded` has non-determinism represented via intrinsic non-determinism
    -- (`ChoiceSemigroup`) of the action
    | ACEmbedded

type family AutomatonFromChoice (ac :: AutomatonChoice) where
    AutomatonFromChoice 'ACNormal = HyperMagicNFA
    AutomatonFromChoice 'ACEmbedded = MagicNFA

newtype AutomatonStepQuantum (ac :: AutomatonChoice) a = AutomatonStepQuantum 
    { getNFA :: AutomatonFromChoice ac a
    }

deriving newtype instance Show (AutomatonFromChoice ac a) 
  => Show (AutomatonStepQuantum ac a)
deriving newtype instance ParallelSemigroup (AutomatonFromChoice ac a) 
  => ParallelSemigroup (AutomatonStepQuantum ac a)
deriving newtype instance OrderedSemigroup (AutomatonFromChoice ac a) 
  => OrderedSemigroup (AutomatonStepQuantum ac a)
deriving newtype instance Pointed (AutomatonFromChoice ac) 
  => Pointed (AutomatonStepQuantum ac)
deriving newtype instance Semigroup (AutomatonFromChoice ac a) 
  => Semigroup (AutomatonStepQuantum ac a)
deriving newtype instance Monoid (AutomatonFromChoice ac a) 
  => Monoid (AutomatonStepQuantum ac a)
deriving newtype instance MonoidStar (AutomatonFromChoice ac a) 
  => MonoidStar (AutomatonStepQuantum ac a)
deriving newtype instance ChoiceSemigroup (AutomatonFromChoice ac a) 
  => ChoiceSemigroup (AutomatonStepQuantum ac a)

    
instance (Ord t, CreatesBellPairs (sq t) op t, ChoiceSemigroup (sq t), Pointed (AutomatonFromChoice 'ACEmbedded))
        => CreatesBellPairs (AutomatonStepQuantum 'ACEmbedded (sq t)) op t where
    tryCreateBellPairFrom = point . tryCreateBellPairFrom

instance (Ord t, Ord (sq t), CreatesBellPairs (NonEmpty (sq t)) op t, Pointed (AutomatonFromChoice 'ACNormal))
        => CreatesBellPairs (AutomatonStepQuantum 'ACNormal (sq t)) op t where
    tryCreateBellPairFrom = foldNonEmpty (<+>) . fmap point . tryCreateBellPairFrom

instance (Ord t, ChoiceSemigroup (sq t), Quantum (sq t) op t, CreatesBellPairs (sq t) op t)
        => Quantum (AutomatonStepQuantum 'ACEmbedded (sq t)) op t where

instance (Ord t, Ord (sq t), ParallelSemigroup (sq t), CreatesBellPairs (NonEmpty (sq t)) op t)
        => Quantum (AutomatonStepQuantum 'ACNormal (sq t)) op t where

instance Layered (AutomatonStepQuantum 'ACEmbedded (sq t)) where
    newtype Layer (AutomatonStepQuantum 'ACEmbedded (sq t)) = OneStep (sq t)
    liftLayer (OneStep s) = point s

instance (ChoiceSemigroup (sq t), Quantum (sq t) op t) 
        => OrderedLayeredQuantum (AutomatonStepQuantum 'ACEmbedded (sq t)) op t where
    orderedTryCreateBellPairFrom = OneStep . tryCreateBellPairFrom

instance (Ord t, ChoiceSemigroup (sq t), Quantum (sq t) op t, OrderedSemigroup (sq t)) 
        => OrderedQuantum (AutomatonStepQuantum 'ACEmbedded (sq t)) op t where

instance (Ord t, Ord (sq t), ParallelSemigroup (sq t), OrderedSemigroup (sq t), CreatesBellPairs (NonEmpty (sq t)) op t) 
        => OrderedQuantum (AutomatonStepQuantum 'ACNormal (sq t)) op t where

instance (Semigroup (sq t)) => OrderedSemigroup (Layer (AutomatonStepQuantum 'ACEmbedded (sq t))) where
   (OneStep s) <.> (OneStep s') = OneStep (s <> s')

instance (Ord tag, Pointed (AutomatonFromChoice ac), Tests (sq tag) test tag)
        => Tests (AutomatonStepQuantum ac (sq tag)) test tag where
    test = point . test

instance (Ord t, Ord (sq t), Tests (sq t) test t, ParallelSemigroup (sq t), OrderedSemigroup (sq t), CreatesBellPairs (NonEmpty (sq t)) op t) 
        => TestsOrderedQuantum (AutomatonStepQuantum 'ACNormal (sq t)) test op t where

instance Tests (sq t) test t => TestsOrderedLayered (AutomatonStepQuantum 'ACEmbedded (sq t)) test t where
    orderedTest = OneStep . test

instance (Ord tag, ChoiceSemigroup (sq tag), TestsQuantum (sq tag) test op tag)
        => TestsOrderedLayeredQuantum (AutomatonStepQuantum 'ACEmbedded (sq tag)) test op tag where

instance (Ord tag, ChoiceSemigroup (sq tag), OrderedSemigroup (sq tag), TestsQuantum (sq tag) test op tag)
        => TestsOrderedQuantum (AutomatonStepQuantum 'ACEmbedded (sq tag)) test op tag where

executeE :: (Ord b, Show b)
    => (a -> b -> Set b)
    -> AutomatonStepQuantum 'ACEmbedded a
    -> b -> Set b
executeE executeStep ahq = fromJust . executeWithE def executeStep ahq

executeWithE :: (Ord b, Show b)
    => ExecutionParams b
    -> (a -> b -> Set b)
    -> AutomatonStepQuantum 'ACEmbedded a
    -> b -> Maybe (Set b)
executeWithE params executeStep (AutomatonStepQuantum nfa) = AE.execute params executeStep nfa

executeWith :: (Ord b, Show b)
    => ExecutionParams b
    -> (a -> b -> Set b)
    -> AutomatonStepQuantum 'ACNormal a
    -> b -> Maybe (Set b)
executeWith params executeStep (AutomatonStepQuantum nfa) = AE.executeHyper params executeStep nfa

execute :: (Ord b, Show b)
    => (a -> b -> Set b)
    -> AutomatonStepQuantum 'ACNormal a
    -> b -> Set b
execute executeStep ahq = fromJust . executeWith def executeStep ahq
