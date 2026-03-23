{-# LANGUAGE StrictData #-}
module BellKAT.Utils.Automata.Guarded
    ( GuardedFA(..)
    , guardedLoop
    , minimizeGuardedFA
    ) where

import qualified Data.IntMap.Strict           as IM
import qualified Data.IntSet                  as IS
import           Data.Pointed
import           Data.Graph
import           Data.List (findIndex, foldl', intercalate, nub)

import           BellKAT.Definitions.Structures.Basic
import           BellKAT.Utils.Automata.Eps
import           BellKAT.Utils.Automata.Transitions
import           BellKAT.Utils.Automata.Transitions.Guarded
import           BellKAT.Utils.Automata.GuardedEps

data GuardedFA t a = GFA
    { gfaInitial :: Int
    , gfaTransition :: GuardedTransitionSystem t a
    } deriving stock (Eq)

data Observation a
    = ObsDone
    | ObsStep a Int
    deriving stock (Eq)

-- | A signature is a mapping from observations to guards
type Signature t a = [(Observation a, t)]

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

instance (Show t, DecidableBoolean t, Eq a) => Guarded t (GuardedFA t a) where
    ite t x y = gefaToGfa $ ite t (gfaToGefa x) (gfaToGefa y)
    while t x = gefaToGfa $ while t (gfaToGefa x)

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

gefaToGfa :: (Show t, DecidableBoolean t, Eq a) => GuardedEpsFA t a -> GuardedFA t a
gefaToGfa (GEFA i t) =
    removeUnreachable . minimizeGuardedFA . removeUnreachable $ GFA i (computeGuardedClosures t)

-- | Two guards are equivalent if neither one allows anything that the other forbids
guardEq :: DecidableBoolean t => t -> t -> Bool
guardEq x y =
    isFalse (x &&* notB y) -- no case where x holds and y doesn't
    && isFalse (y &&* notB x) -- and vice versa

-- | Returns the guard for a given observation in a signature
lookupObservation :: (Boolean t, Eq a) => Observation a -> Signature t a -> t
lookupObservation _ [] = false
lookupObservation obs ((obs', t) : rest)
    | obs == obs' = t
    | otherwise = lookupObservation obs rest

-- | Inserts an observation into a signature, merging with existing observations if necessary
insertObservation
    :: (Show t, DecidableBoolean t, Eq a)
    => Observation a
    -> t
    -> Signature t a
    -> Signature t a
insertObservation _ t sig
    | isFalse t = sig
insertObservation obs t [] = [(obs, t)]
insertObservation obs t ((obs', t') : rest)
    | obs == obs' =
        let merged = t ||* t'
         in if isFalse merged then rest else (obs, merged) : rest
    | otherwise = (obs', t') : insertObservation obs t rest

-- | Computes the signature of a state given a partition of states into blocks
stateSignature
    :: (Show t, DecidableBoolean t, Eq a)
    => IM.IntMap Int
    -> GuardedTransitionSystem t a
    -> Int
    -> Signature t a
stateSignature partition tr =
    foldl' addObservation [] . gTransitionsToList . (tr !)
  where
    addObservation sig (t, Done) =
        insertObservation ObsDone t sig
    addObservation sig (t, Step a j) =
        insertObservation (ObsStep a (partition IM.! j)) t sig

-- | Decides whether two signatures describe the same one-step behaviour
-- i.e., Two signatures are the same if they have the same guards for each observation
sameSignature
    :: (DecidableBoolean t, Eq a)
    => Signature t a
    -> Signature t a
    -> Bool
sameSignature xs ys =
    all sameGuard (nub (map fst xs <> map fst ys))
  where
    sameGuard obs = guardEq (lookupObservation obs xs) (lookupObservation obs ys)

-- | One refinement round, which refines a partition of states by splitting blocks according to their signatures
refinePartition
    :: (Show t, DecidableBoolean t, Eq a)
    => GuardedFA t a
    -> IM.IntMap Int
    -> IM.IntMap Int
refinePartition fa partition =
    snd $ foldl' place ([], IM.empty) (IS.toAscList $ states fa)
  where
    tr = gfaTransition fa

    place (signatures, blocks) state =
        let signature = stateSignature partition tr state
         in case findIndex (sameSignature signature) signatures of
                Just block ->
                    (signatures, IM.insert state block blocks)
                Nothing ->
                    ( signatures <> [signature]
                    , IM.insert state (length signatures) blocks
                    )

-- | Iteratively refines a partition until it stabilizes, returning the final partition
fixPartition
    :: (Show t, DecidableBoolean t, Eq a)
    => GuardedFA t a
    -> IM.IntMap Int
fixPartition fa = go (IM.fromSet (const 0) (states fa))
  where
    go partition =
        let partition' = refinePartition fa partition
         in if partition' == partition then partition else go partition'

-- | Converts a signature to a list of guarded transitions
signatureTransitions
    :: (Show t, DecidableBoolean t, Eq a)
    => Signature t a
    -> GuardedTransitions t a
signatureTransitions =
    gTransitionsFromList . fmap toTransition
  where
    toTransition (ObsDone, t) = (t, Done)
    toTransition (ObsStep a block, t) = (t, Step a block)

minimizeGuardedFA
    :: (Show t, DecidableBoolean t, Eq a)
    => GuardedFA t a
    -> GuardedFA t a
minimizeGuardedFA fa =
    let partition = fixPartition fa
        representatives =
            IM.foldlWithKey'
                (\acc state block -> IM.insertWith (\_ old -> old) block state acc)
                IM.empty
                partition
        tr = gfaTransition fa
     in GFA
            (partition IM.! gfaInitial fa)
            (gtsFromList
                [ ( block
                  , gTransitionsToList
                    . signatureTransitions
                    $ stateSignature partition tr state
                  )
                | (block, state) <- IM.toAscList representatives
                ])

gfaToGefa :: GuardedFA t a -> GuardedEpsFA t a
gfaToGefa (GFA i t) = GEFA i (fmap Right t)

instance (Show t, DecidableBoolean t, Eq a) => Semigroup (GuardedFA t a) where
    a <> b = gefaToGfa (gfaToGefa a <> gfaToGefa b)

instance (Show t, DecidableBoolean t, Eq a) => Monoid (GuardedFA t a) where
    mempty = GFA 0 (fromTransitions 0 $ gTransitionsSingleton true Done)

productWith :: (Show t, Show a, DecidableBoolean t, Eq a) => (a -> a -> a) -> GuardedFA t a -> GuardedFA t a -> GuardedFA t a
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

-- | TODO: detect wether body has at least one action
guardedLoop 
    :: (Show t, DecidableBoolean t, Eq a) 
    => t -- | guard for the loop
    -> GuardedFA t a -- | body of the loop
    -> GuardedFA t a
guardedLoop t body = 
    let GEFA bI bT = gfaToGefa body -- | add epsilons for looping
        bT' = shiftUp 1 bT -- | shift for the body states of 1 to make room for the new initial state
        rewired = setDoneToStep (Left Eps) 0 bT' -- | rewire body to loop back to the start (0 is the new initial state and bT' is the shifted body)
        -- | now, from the new head, transition to the body if the guard holds, and transition to done if the guard doesn't hold
        headT = gtsFromList 
            [(0, [ (t, Step (Left Eps) (bI + 1)) 
                 , (notB t, Done)
                 ])]
        loopsEpsFA = GEFA 0 (headT <> rewired)
        in gefaToGfa loopsEpsFA

instance (Show t, Show a, DecidableBoolean t, Eq a, ParallelSemigroup a) =>  ParallelSemigroup (GuardedFA t a) where
    p <||> q = productWith (<||>) p q 

instance (Show t, Show a, DecidableBoolean t, Eq a, OrderedSemigroup a) =>  OrderedSemigroup (GuardedFA t a) where
    p  <.> q = productWith (<.>) p q 
