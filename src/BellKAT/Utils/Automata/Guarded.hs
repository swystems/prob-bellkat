{-# LANGUAGE StrictData #-}
module BellKAT.Utils.Automata.Guarded
    ( GuardedFA(..)
    ) where

import qualified Data.IntSet                  as IS
import           Data.Pointed
import           Data.Graph
import           Data.List (intercalate)

import           BellKAT.Definitions.Structures.Basic
import           BellKAT.Utils.Automata.Eps
import           BellKAT.Utils.Automata.Transitions
import           BellKAT.Utils.Automata.Transitions.Guarded
import           BellKAT.Utils.Automata.GuardedEps

data GuardedFA t a = GFA
    { gfaInitial :: Int
    , gfaTransition :: GuardedTransitionSystem t a
    } deriving stock (Eq)

instance (Show a, Show t, DecidableBoolean t) => Show (GuardedFA t a) where
    show x = intercalate "\n" $
        map showState $ toPairs (gfaTransition x)
      where
        showState (s, sTr) = 
            (if s == gfaInitial x then "^" else "") 
            <> show s 
            <> (if length (toTransitionsList sTr) <= 1 then ": " else ":\n")
            <> showGuardedTransitionsWith showGuardedTransition sTr

instance HasStates (GuardedFA t a) where
  states = states . gfaTransition
  numStates = numStates . gfaTransition

instance CanMapStates (GuardedFA t a) where
    mapStates f (GFA i t) = GFA (f i) (mapStates f t)
    mapStatesMonotonic f (GFA i t) = GFA (f i) (mapStatesMonotonic f t)

instance CanRestrictStates (GuardedFA t a) where
  restrictStates s (GFA i t) = 
      if not (i `statesMember` s)
         then error "Cannot restricted outside initial state"
         else GFA i (restrictStates s t)

instance Boolean t => LikeAutomaton (GuardedFA t) where
  type AutomatonAction (GuardedFA t) a = a
  type AutomatonTS (GuardedFA t) = GuardedTransitionSystem t
  initialState = gfaInitial
  transitionSystem = gfaTransition

instance (Show t, DecidableBoolean t) => Pointed (GuardedFA t) where
    point x = GFA 
        0 
        (singletonGts 0 true x 1 <> singletonDoneGts true 1)

instance (Show t, DecidableBoolean t) => Guarded t (GuardedFA t a) where
    ite t x y = gefaToGfa $ ite t (gfaToGefa x) (gfaToGefa y)

computeGuardedClosures
    :: (Show t, DecidableBoolean t)
    => GuardedTransitionSystem t (Either Eps a)
    -> GuardedTransitionSystem t a
computeGuardedClosures tr = 
    let g = epsTransitionToGraph tr
        ts = map ensureSingleton $ scc g
     in constructClosure tr ts mempty
  where
    ensureSingleton :: Tree Vertex -> Vertex
    ensureSingleton (Node x []) = x
    ensureSingleton _ = error "SCC has cycles"

    constructClosure 
        :: (Show t, DecidableBoolean t)
        => GuardedTransitionSystem t (Either Eps a)
        -> [Vertex] 
        -> GuardedTransitionSystem t a
        -> GuardedTransitionSystem t a
    constructClosure _ [] acc = acc
    constructClosure epsTS (i:xs) acc = 
        let opts = mconcat $ map (expandEpsilons acc) $ gTransitionsToList $ epsTS ! i
         in constructClosure epsTS xs (acc <> fromTransitions i opts)

    expandEpsilons 
        :: (Show t, DecidableBoolean t)
        => GuardedTransitionSystem t a
        -> (t, Next (Either Eps a)) 
        -> GuardedTransitions t a
    expandEpsilons _ (t, Done) = gTransitionsSingleton t Done
    expandEpsilons _ (t, Step (Right act) j) = gTransitionsSingleton t (Step act j)
    expandEpsilons acc (t, Step (Left Eps) j) = mapTests (&&* t) $ acc ! j

epsTransitionToGraph 
    :: (Show t, DecidableBoolean t) 
    => GuardedTransitionSystem t (Either Eps a) -> Graph
epsTransitionToGraph tr = 
    buildG (IS.findMin . states $ tr, IS.findMax . states $ tr) 
        [ (i, j) 
        | (i, trI) <- toPairs tr
        , (_, Step (Left Eps) j) <- gTransitionsToList trI
        ]

gefaToGfa :: (Show t, DecidableBoolean t) => GuardedEpsFA t a -> GuardedFA t a
gefaToGfa (GEFA i t) =
    removeUnreachable $ GFA i (computeGuardedClosures t)

gfaToGefa :: GuardedFA t a -> GuardedEpsFA t a
gfaToGefa (GFA i t) = GEFA i (fmap Right t)

instance (Show t, DecidableBoolean t) => Semigroup (GuardedFA t a) where
    a <> b = gefaToGfa (gfaToGefa a <> gfaToGefa b)

instance (Show t, DecidableBoolean t) => Monoid (GuardedFA t a) where
    mempty = GFA 0 (fromTransitions 0 $ gTransitionsSingleton true Done)

productWith :: (Show t, Show a, DecidableBoolean t) => (a -> a -> a) -> GuardedFA t a -> GuardedFA t a -> GuardedFA t a
productWith f (GFA aI aT) (GFA bI bT) = 
    let aSize = numStates aT
        aF = undefined <$ filterNext isDone aT
        bF = undefined <$ filterNext isDone bT
        aFinal = numStates aT
        bFinal = numStates bT
        encode x y = x + y * (aSize + 1) -- accounting for final
        bothT = productWithStates f encode (filterNext isStep aT) (filterNext isStep bT)
        aFinalT = stepFinalLeft encode aFinal aF (setStepToDone . filterNext isStep $ () <$ bT)
        bFinalT = stepFinalRight encode (setStepToDone . filterNext isStep $ () <$ aT) bFinal bF
        bothFinalT = undefined <$ productWithStates undefined encode aF bF
        onlyA = mapStatesMonotonic (`encode` bFinal) aT
        onlyB = mapStatesMonotonic (aFinal `encode`) bT
     in gefaToGfa $ GEFA
           (encode aI bI)
           (fmap Right bothT 
                <> aFinalT
                <> bFinalT
                <> fmap Right bothFinalT
                <> fmap Right onlyA
                <> fmap Right onlyB)
  where
    stepFinalLeft 
        :: (Show t, DecidableBoolean t)
        => (State -> State -> State) 
        -> State -- | where to exit on the left
        -> GuardedTransitionSystem t ()  -- | where to exit from on the left
        -> GuardedTransitionSystem t () -- | where to keep the right side
        -> GuardedTransitionSystem t (Either Eps a)
    stepFinalLeft encode lFinal lF rF =
        Left Eps <$ productStates encode (setDoneToStep () lFinal lF) (loopDone rF)

    stepFinalRight 
        :: (Show t, DecidableBoolean t)
        => (State -> State -> State) 
        -> GuardedTransitionSystem t () -- | where to keep the left side
        -> State -- | where to exit on the right 
        -> GuardedTransitionSystem t () -- | where to exti from on the right
        -> GuardedTransitionSystem t (Either Eps a)
    stepFinalRight encode lS rFinal rF = stepFinalLeft (flip encode) rFinal rF lS

instance (Show t, Show a, DecidableBoolean t, ParallelSemigroup a) =>  ParallelSemigroup (GuardedFA t a) where
    p <||> q = productWith (<||>) p q 

instance (Show t, Show a, DecidableBoolean t, OrderedSemigroup a) =>  OrderedSemigroup (GuardedFA t a) where
    p  <.> q = productWith (<.>) p q 
