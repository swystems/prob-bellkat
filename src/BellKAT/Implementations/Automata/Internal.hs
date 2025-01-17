{-# LANGUAGE TupleSections #-}
module BellKAT.Implementations.Automata.Internal where

import           Data.Tuple                   (swap)
import           Data.Bifunctor               (second)
import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IS
import           Data.These
import           Data.Graph

import BellKAT.Definitions.Structures.Basic

data Eps = Eps deriving stock (Show, Eq)

instance (ChoiceSemigroup a) => ChoiceSemigroup (These Eps a) where
    This Eps <+> This Eps       = This Eps
    This Eps <+> That x         = These Eps x
    This Eps <+> These Eps x    = These Eps x
    That x <+> This Eps         = These Eps x
    That x <+> That y           = That (x <+> y)
    That x <+> These Eps y      = These Eps (x <+> y)
    These Eps x <+> These Eps y = These Eps (x <+> y)
    These Eps x <+> This Eps    = These Eps x
    These Eps x <+> That y      = These Eps (x <+> y)

showTransition :: Show a => (Int, a) -> String
showTransition (j , act) = "-( " <> show act <> " )-> " <> show j

newtype Transitions a = 
    T { unT :: IntMap a } deriving newtype Eq
newtype TransitionSystem a = 
    TS { unTS :: IntMap (Transitions a) } deriving newtype Eq
type States = IntSet

numStates :: TransitionSystem a -> Int
numStates = IM.size . unTS

filterStates :: (Int -> Bool) -> States -> States
filterStates = IS.filter

filterTransitions :: (a -> Bool) -> Transitions a -> Transitions a
filterTransitions f = T . IM.filter f . unT

filterTS :: (a -> Bool) -> TransitionSystem a -> TransitionSystem a
filterTS f = TS . IM.map (filterTransitions f) . unTS

emptyTransitions :: Transitions a
emptyTransitions = T mempty

mapMaybeTransitions :: (a -> Maybe b) -> Transitions a -> Transitions b
mapMaybeTransitions f = T . IM.mapMaybe f . unT

mapMaybeTS :: (a -> Maybe b) -> TransitionSystem a -> TransitionSystem b
mapMaybeTS f = TS . IM.map (mapMaybeTransitions f) . unTS

singletonTransition :: a -> Int -> Transitions a
singletonTransition a i = T $ IM.singleton i a

singletonTS :: Int -> a -> Int -> TransitionSystem a
singletonTS i a j = TS $ 
    IM.singleton i (singletonTransition a j) 
    <> (if i == j then mempty else IM.singleton j emptyTransitions)

singletonState :: Int -> States
singletonState = IS.singleton

statesFromList :: [Int] -> States
statesFromList = IS.fromList

intersection :: States -> States -> States 
intersection = IS.intersection

(!) :: TransitionSystem a -> Int -> Transitions a 
(TS ts) ! k = ts IM.! k 

fromStates :: (Int -> Transitions a) -> States -> TransitionSystem a
fromStates f = TS . IM.fromSet f 

toTransitionsList :: TransitionSystem a -> [(Int, Transitions a)]
toTransitionsList = IM.toList . unTS

loopStates :: States -> TransitionSystem ()
loopStates = TS . IM.fromSet (singletonTransition ())

computeClosure :: TransitionSystem () -> TransitionSystem ()
computeClosure ts =
    let g = tsToGraph ts
     in TS $ IM.fromSet (transitionsFromList . map ((),) . reachable g) $ IM.keysSet $ unTS ts

computeClosureTransition
    :: (ChoiceSemigroup a)
    => TransitionSystem a -> States -> Transitions a
computeClosureTransition (TS tr) s =
    mconcat . map snd . IM.toList $ IM.restrictKeys tr s

tsToGraph :: TransitionSystem () -> Graph
tsToGraph (TS ts) =
    buildG (IS.findMin . IM.keysSet $ ts, IS.findMax . IM.keysSet $ ts)
        [ (i, j)
        | (i, trI) <- IM.toList ts
        , (_, j) <- transitionsToList trI
        ]

transitionsFromList :: [(a, Int)] -> Transitions a
transitionsFromList = T . IM.fromList . map swap

transitionsToList :: Transitions a -> [(a, Int)]
transitionsToList = map swap . IM.toList . unT

tsFromList :: [(Int, [(a, Int)])] -> TransitionSystem a
tsFromList = TS . IM.fromList . map (second transitionsFromList)

sendStatesInto :: a -> Int -> States -> TransitionSystem a
sendStatesInto a i = TS . IM.fromSet (const $ singletonTransition a i)

isFinal :: Int -> States -> Bool
isFinal = IS.member

class HasStates a where
    states :: a -> States

instance HasStates (TransitionSystem a) where
    states = IM.keysSet . unTS

instance HasStates (Transitions a) where
    states = IM.keysSet . unT

class CanMapStates a where
    mapStates :: (Int -> Int) -> a -> a
    mapStatesMonotonic :: (Int -> Int) -> a -> a
    mapStatesMonotonic = mapStates

instance CanMapStates States where
    mapStates = IS.map

class CanMapStatesMonotonic a where

instance CanMapStates (Transitions a) where
    mapStates f = T . IM.mapKeys f . unT
    mapStatesMonotonic f = T . IM.mapKeysMonotonic f . unT

instance CanMapStates (TransitionSystem a) where
    mapStates f = TS . IM.mapKeys f . IM.map (mapStates f) . unTS
    mapStatesMonotonic f = TS . IM.mapKeysMonotonic f . IM.map (mapStatesMonotonic f) . unTS

class CanShiftUp a where
    shiftUp :: Int -> a -> a

instance CanShiftUp States where
    shiftUp k = IS.mapMonotonic (+ k)

instance CanShiftUp (Transitions a) where
    shiftUp k = T . IM.mapKeysMonotonic (+ k) . unT

instance CanShiftUp (TransitionSystem a) where
    shiftUp k = TS . IM.map (shiftUp k) . IM.mapKeysMonotonic (+k) . unTS

instance ChoiceSemigroup a => Semigroup (Transitions a) where
    (T xs) <> (T ys) = T $ IM.unionWith (<+>) xs ys

instance ChoiceSemigroup a => Monoid (Transitions a) where
    mempty = T mempty

instance ChoiceSemigroup a => Semigroup (TransitionSystem a) where
    (TS xs) <> (TS ys) = TS $ IM.unionWith (<>) xs ys

instance ChoiceSemigroup a => Monoid (TransitionSystem a) where
    mempty = TS mempty

showTransitionsWith :: ((Int, a) -> String) -> Transitions a -> String
showTransitionsWith f = unlines . map f . IM.toList . unT

instance Show a => Show (TransitionSystem a) where
    show x = unlines $ map showState $ toTransitionsList x
      where
        showState (s, sTr) = 
            show s 
            <> ":\n"
            <> showTransitionsWith showTransition sTr

instance Show a => Show (Transitions a) where
    show = showTransitionsWith showTransition

class CanRestrictStates a where
    restrictStates :: States -> a -> a

instance CanRestrictStates States where
   restrictStates = IS.intersection

instance CanRestrictStates (Transitions a) where
   restrictStates s (T tr) = T $ tr `IM.restrictKeys` s

instance CanRestrictStates (TransitionSystem a) where
   restrictStates s (TS tr) = TS $ fmap (restrictStates s) $ tr `IM.restrictKeys` s

instance Functor Transitions where
    fmap f = T . IM.map f . unT

instance Functor TransitionSystem where
    fmap f = TS . IM.map (fmap f) . unTS

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
