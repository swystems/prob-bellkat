{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
module BellKAT.Utils.Automata.Transitions 
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
    -- * Transitions
    , Transitions
    , LikeTransitions(..)
    , emptyTransitions
    , singletonTransitions
    , transitionsToList
    , transitionsFromList
    , filterTransitions
    , mapMaybeTransitions
    , showTransition
    , showTransitionsWith
    -- * Transition systems
    , LikeTransitionSystem(..)
    , TransitionSystem
    , singletonTs
    , tsFromList
    , tsFromStates
    , filterTS
    , mapMaybeTS
    , sendStatesInto
    , computeClosure
    , computeClosureTransition
    -- * Extra transition manipulation utilities
    , shiftUp
    , CanProductWithStates(..)
    , productStates
    -- * Automaton Class
    , LikeAutomaton(..)
    , removeUnreachable
    ) where

import           Data.Kind
import           Data.Tuple                   (swap)
import           Data.Bifunctor               (second)

import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IS
import           Data.Graph

import BellKAT.Definitions.Structures.Basic

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

class HasStates1 t where
    states1 :: t a -> States
    numStates1 :: t a -> Int

instance HasStates States where
    states = id
    numStates = IS.size

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

newtype Transitions a = 
    T { unT :: IntMap a } deriving newtype (Eq, Functor)

class LikeTransitions t where
    toTransitionsList :: t a -> [(a, State)]

instance LikeTransitions Transitions where
    toTransitionsList = map swap . IM.toList . unT

emptyTransitions :: Transitions a
emptyTransitions = T mempty

singletonTransitions :: a -> Int -> Transitions a
singletonTransitions a i = T $ IM.singleton i a

transitionsFromList :: [(a, Int)] -> Transitions a
transitionsFromList = T . IM.fromList . map swap

transitionsToList :: Transitions a -> [(a, Int)]
transitionsToList = map swap . IM.toList . unT


filterTransitions :: (a -> Bool) -> Transitions a -> Transitions a
filterTransitions f = T . IM.filter f . unT

mapMaybeTransitions :: (a -> Maybe b) -> Transitions a -> Transitions b
mapMaybeTransitions f = T . IM.mapMaybe f . unT

showTransitionsWith :: ((Int, a) -> String) -> Transitions a -> String
showTransitionsWith f = unlines . map f . IM.toList . unT

instance Show a => Show (Transitions a) where
    show = showTransitionsWith showTransition

showTransition :: Show a => (Int, a) -> String
showTransition (j , act) = "-( " <> show act <> " )-> " <> show j

instance HasStates (Transitions a) where
    states = IM.keysSet . unT
    numStates = IM.size . unT

instance CanMapStates (Transitions a) where
    mapStates f = T . IM.mapKeys f . unT
    mapStatesMonotonic f = T . IM.mapKeysMonotonic f . unT

instance CanRestrictStates (Transitions a) where
   restrictStates s (T tr) = T $ tr `IM.restrictKeys` s

instance ChoiceSemigroup a => Semigroup (Transitions a) where
    (T xs) <> (T ys) = T $ IM.unionWith (<+>) xs ys

instance ChoiceSemigroup a => Monoid (Transitions a) where
    mempty = T mempty

newtype TransitionSystem a = 
    TS { unTS :: IntMap (Transitions a) } deriving newtype Eq

class (HasStates1 t, Functor t, LikeTransitions (TSTransitions t)) => LikeTransitionSystem t where
    type TSTransitions t :: Type -> Type
    toListOfTransitions :: t a -> [(Int, TSTransitions t a)]
    fromTransitions :: Int -> TSTransitions t a -> t a
    loopStates :: States -> t ()
    (!) :: t a -> Int -> TSTransitions t a

singletonTs :: Int -> a -> Int -> TransitionSystem a
singletonTs i a j = TS $ 
    IM.singleton i (singletonTransitions a j) 
    <> (if i == j then mempty else IM.singleton j emptyTransitions)

tsFromList :: [(Int, [(a, Int)])] -> TransitionSystem a
tsFromList = TS . IM.fromList . map (second transitionsFromList)

tsFromStates :: (Int -> Transitions a) -> States -> TransitionSystem a
tsFromStates f = TS . IM.fromSet f 

filterTS :: (a -> Bool) -> TransitionSystem a -> TransitionSystem a
filterTS f = TS . IM.map (filterTransitions f) . unTS

mapMaybeTS :: (a -> Maybe b) -> TransitionSystem a -> TransitionSystem b
mapMaybeTS f = TS . IM.map (mapMaybeTransitions f) . unTS

instance LikeTransitionSystem TransitionSystem where
    type TSTransitions TransitionSystem = Transitions
    fromTransitions i ts = TS $
        IM.singleton i ts <> IM.fromSet (const emptyTransitions) (states ts)
    toListOfTransitions = IM.toList . unTS
    loopStates = TS . IM.fromSet (singletonTransitions ())
    (TS ts) ! k = ts IM.! k 

sendStatesInto :: a -> Int -> States -> TransitionSystem a
sendStatesInto a i = TS . IM.fromSet (const $ singletonTransitions a i)

computeClosure :: TransitionSystem () -> TransitionSystem ()
computeClosure ts =
    let g = tsToGraph ts
     in TS $ IM.fromSet (transitionsFromList . map ((),) . reachable g) $ IM.keysSet $ unTS ts

tsToGraph :: LikeTransitionSystem t => t a -> Graph
tsToGraph ts =
    buildG (IS.findMin . states1 $ ts, IS.findMax . states1 $ ts)
        [ (i, j)
        | (i, trI) <- toListOfTransitions ts
        , (_, j) <- toTransitionsList trI
        ]

computeClosureTransition
    :: (ChoiceSemigroup a)
    => TransitionSystem a -> States -> Transitions a
computeClosureTransition (TS tr) s =
    mconcat . map snd . IM.toList $ IM.restrictKeys tr s

instance Functor TransitionSystem where
    fmap f = TS . IM.map (fmap f) . unTS

instance Show a => Show (TransitionSystem a) where
    show x = unlines $ map showState $ toListOfTransitions x
      where
        showState (s, sTr) = 
            show s 
            <> ":\n"
            <> showTransitionsWith showTransition sTr

instance CanRestrictStates (TransitionSystem a) where
   restrictStates s (TS tr) = TS $ fmap (restrictStates s) $ tr `IM.restrictKeys` s

instance HasStates1 TransitionSystem where
    states1 = states
    numStates1 = numStates

instance HasStates (TransitionSystem a) where
    states = IM.keysSet . unTS
    numStates = IM.size . unTS

instance CanMapStates (TransitionSystem a) where
    mapStates f = TS . IM.mapKeys f . IM.map (mapStates f) . unTS
    mapStatesMonotonic f = TS . IM.mapKeysMonotonic f . IM.map (mapStatesMonotonic f) . unTS

instance ChoiceSemigroup a => Semigroup (TransitionSystem a) where
    (TS xs) <> (TS ys) = TS $ IM.unionWith (<>) xs ys

instance ChoiceSemigroup a => Monoid (TransitionSystem a) where
    mempty = TS mempty

shiftUp :: CanMapStates a => Int -> a -> a
shiftUp k = mapStatesMonotonic (+k)

class CanProductWithStates t where
    productWithStates :: (a -> a -> a) -> (Int -> Int -> Int) -> t a -> t a -> t a

instance CanProductWithStates Transitions where
    productWithStates f encode atr btr = T $ IM.fromList
        [ (encode aTo bTo, f aAct bAct )
        | (aTo, aAct) <- IM.toList . unT $ atr
        , (bTo, bAct) <- IM.toList . unT $ btr]

instance CanProductWithStates TransitionSystem where
    productWithStates f encode aT bT = TS $ IM.fromList
        [ (encode aFrom bFrom, productWithStates f encode aTo bTo)
        | (aFrom, aTo) <- IM.toList . unTS $ aT
        , (bFrom, bTo) <- IM.toList . unTS $ bT
        ]

productStates :: CanProductWithStates t => (Int -> Int -> Int) -> t () -> t () -> t ()
productStates = productWithStates (\() () -> ())

class LikeTransitionSystem (AutomatonTS t) => LikeAutomaton t where
    type AutomatonAction t a :: Type
    type AutomatonTS t :: Type -> Type
    initialState :: t a -> State
    transitionSystem :: t a -> AutomatonTS t (AutomatonAction t a)

findReachable :: LikeTransitionSystem t 
              => State -> t a -> States
findReachable sInit ts = IS.fromList $ reachable (tsToGraph ts) sInit

removeUnreachable :: (CanMapStates (t a), CanRestrictStates (t a), LikeAutomaton t) 
                  => t a -> t a
removeUnreachable fa = 
    let newStates = findReachable (initialState fa) (transitionSystem fa)
        oldToNew = IS.foldl 
            (\m k -> if k `IS.member` newStates then IM.insert k (IM.size m) m else m) 
            mempty (states fa)
     in mapStatesMonotonic (oldToNew IM.!) $ restrictStates newStates fa
