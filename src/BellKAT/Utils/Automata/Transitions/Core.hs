module BellKAT.Utils.Automata.Transitions.Core
    (
    -- * Basic definitions for states of transition systems
      State
    , States
    , HasStates(..)
    , HasStates1(..)
    , CanMapStates(..)
    , CanRestrictStates(..)
    , filterStates
    , singletonState
    , statesFromList
    , statesIntersection
    , statesMember
    -- * Common classes for transitions and transition systems
    , LikeTransitions(..)
    , LikeTransitionSystem(..)
    ) where

import           Data.Kind
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IS

type State = Int
type States = IntSet

filterStates :: (State -> Bool) -> States -> States
filterStates = IS.filter

singletonState :: State -> States
singletonState = IS.singleton

statesFromList :: [State] -> States
statesFromList = IS.fromList

statesIntersection :: States -> States -> States 
statesIntersection = IS.intersection

statesMember :: State -> States -> Bool
statesMember = IS.member

class HasStates a where
    states :: a -> States
    numStates :: a -> Int

instance HasStates States where
    states = id
    numStates = IS.size

class HasStates1 t where
    states1 :: t a -> States
    numStates1 :: t a -> Int

class HasStates a => CanMapStates a where
    mapStates :: (State -> State) -> a -> a
    mapStatesMonotonic :: (State -> State) -> a -> a
    mapStatesMonotonic = mapStates

instance CanMapStates States where
    mapStates = IS.map

class HasStates a => CanRestrictStates a where
    restrictStates :: States -> a -> a

instance CanRestrictStates States where
   restrictStates = IS.intersection

class LikeTransitions t where
    toTransitionsList :: t a -> [(a, State)]

class (HasStates1 t, Functor t, LikeTransitions (TSTransitions t)) => LikeTransitionSystem t where
    type TSTransitions t :: Type -> Type
    toListOfTransitions :: t a -> [(Int, TSTransitions t a)]
    fromTransitions :: Int -> TSTransitions t a -> t a
    loopStates :: States -> t ()
    (!) :: t a -> Int -> TSTransitions t a
