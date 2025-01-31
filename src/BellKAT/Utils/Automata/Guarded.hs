{-# LANGUAGE StrictData #-}
module BellKAT.Utils.Automata.Guarded
    ( GuardedFA(..)
    ) where

import qualified Data.IntSet                  as IS
import           Data.Pointed
import           Data.Graph

import           BellKAT.Definitions.Structures.Basic
import           BellKAT.Utils.Automata.Eps
import           BellKAT.Utils.Automata.Transitions
import           BellKAT.Utils.Automata.Transitions.Guarded
import           BellKAT.Utils.Automata.GuardedEps

data GuardedFA t a = GFA
    { gfaInitial :: Int
    , gfaTransition :: GuardedTransitionSystem t a
    }

instance DecidableBoolean t => Pointed (GuardedFA t) where
    point x = GFA 
        0 
        (singletonGts 0 true x 1 <> singletonDoneGts true 1)

computeGuardedClosures
    :: DecidableBoolean t
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
        :: DecidableBoolean t
        => GuardedTransitionSystem t (Either Eps a)
        -> [Vertex] 
        -> GuardedTransitionSystem t a
        -> GuardedTransitionSystem t a
    constructClosure _ [] acc = acc
    constructClosure epsTS (i:xs) acc = 
        let opts = mconcat $ map (expandEpsilons acc) $ gTransitionsToList $ epsTS ! i
         in constructClosure epsTS xs (acc <> fromTransitions i opts)

    expandEpsilons 
        :: DecidableBoolean t
        => GuardedTransitionSystem t a
        -> (t, Next (Either Eps a)) 
        -> GuardedTransitions t a
    expandEpsilons _ (t, Done) = gTransitionsSingleton t Done
    expandEpsilons _ (t, Step (Right act) j) = gTransitionsSingleton t (Step act j)
    expandEpsilons acc (t, Step (Left Eps) j) = mapTests (&&* t) $ acc ! j

epsTransitionToGraph :: Boolean t => GuardedTransitionSystem t (Either Eps a) -> Graph
epsTransitionToGraph tr = 
    buildG (IS.findMin . states $ tr, IS.findMax . states $ tr) 
        [ (i, j) 
        | (i, trI) <- toTransitionsList tr
        , (_, Step (Left Eps) j) <- gTransitionsToList trI
        ]

gefaToGfa :: DecidableBoolean t => GuardedEpsFA t a -> GuardedFA t a
gefaToGfa (GEFA i t) =
    GFA i (computeGuardedClosures t)

gfaToGefa :: GuardedFA t a -> GuardedEpsFA t a
gfaToGefa (GFA i t) = GEFA i (fmap Right t)

instance DecidableBoolean t => Semigroup (GuardedFA t a) where
    a <> b = gefaToGfa (gfaToGefa a <> gfaToGefa b)

productWith :: DecidableBoolean t => (a -> a -> a) -> GuardedFA t a -> GuardedFA t a -> GuardedFA t a
productWith f (GFA aI aT) (GFA bI bT) = 
    let aSize = numStates aT
        aF = () <$ filterNext isDone aT
        bF = () <$ filterNext isDone bT
        aFinal = numStates aT
        bFinal = numStates bT
        encode x y = x + y * (aSize + 1) -- accounting for final
        bothT = productWithStates f encode aT bT
        aFinalT = stepFinalLeft encode aFinal aF (states bT)
        bFinalT = stepFinalRight encode (states aT) bFinal bF
        onlyA = mapStatesMonotonic (`encode` bFinal) aT
        onlyB = mapStatesMonotonic (aFinal `encode`) bT
        newAF = Left Eps <$ mapStates (`encode` bFinal) aF
        newBF = Left Eps <$ mapStates (aFinal `encode`) bF
     in gefaToGfa $ GEFA
           (encode aI bI)
           (fmap Right bothT 
                <> aFinalT
                <> bFinalT
                <> fmap Right onlyA
                <> fmap Right onlyB
                <> newAF
                <> newBF)
  where
    stepFinalLeft 
        :: DecidableBoolean t 
        => (Int -> Int -> Int) 
        -> Int -> GuardedTransitionSystem t () -> States -> GuardedTransitionSystem t (Either Eps a)
    stepFinalLeft encode lFinal lF rS =
        Left Eps <$ productStates encode (setDoneToStep () lFinal lF) (loopStates rS)

    stepFinalRight 
        :: DecidableBoolean t 
        => (Int -> Int -> Int) 
        -> States -> Int -> GuardedTransitionSystem t () -> GuardedTransitionSystem t (Either Eps a)
    stepFinalRight encode lS rFinal rF = stepFinalLeft (flip encode) rFinal rF lS

instance (DecidableBoolean t, ParallelSemigroup a) =>  ParallelSemigroup (GuardedFA t a) where
    p  <||> q = productWith (<||>) p q 

instance (DecidableBoolean t, OrderedSemigroup a) =>  OrderedSemigroup (GuardedFA t a) where
    p  <.> q = productWith (<.>) p q 
