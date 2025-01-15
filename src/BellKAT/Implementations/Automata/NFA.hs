{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
module BellKAT.Implementations.Automata.NFA 
    ( MagicNFA(..)
    , HyperMagicNFA(..)
    , restrictStates
    , productWith
    , showStateId
    ) where

import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IS
import           Data.List
import qualified Data.Map.Strict              as Map
import           Data.Pointed
import qualified Data.Set                     as Set
import           Data.Set                     (Set)
import           Data.Foldable (toList)
import           Data.These
import           Data.These.Combinators       (isThat, justThat)
import           Data.Graph

import           BellKAT.Implementations.Automata.Internal
import           BellKAT.Implementations.Automata.EpsNFA
import           BellKAT.Definitions.Structures.Basic

data MagicNFA a = MNFA
    { mnfaInitial    :: Int
    , mnfaTransition :: !(IntMap (IntMap a))
    , mnfaFinal      :: !IntSet
    }

restrictStates :: MagicNFA a -> IntSet -> MagicNFA a
restrictStates x states = MNFA 
    { mnfaInitial = mnfaInitial x
    , mnfaTransition = fmap (`IM.restrictKeys` states) $ mnfaTransition x `IM.restrictKeys` states
    , mnfaFinal = states `IS.intersection` mnfaFinal x
    }

instance Show a => Show (MagicNFA a) where
    show x = 
        concatMap showState $ IM.toList (mnfaTransition x)
      where
        showState (s, sTr) = 
            showStateId x s
            <> ":\n"
            <> unlines (map showTransition $ IM.toList sTr)

showStateId :: MagicNFA a -> Int -> String
showStateId x s = 
    (if s == mnfaInitial x then "^" else "") 
    <> show s 
    <> (if IS.member s (mnfaFinal x) then "$" else "")

computeClosures :: IntMap (IntMap (These Eps a)) -> IntMap IntSet
computeClosures tr = 
    let g = epsTransitionToGraph tr
     in IM.fromSet (IS.fromList . reachable g) $ IM.keysSet tr

epsTransitionToGraph :: IntMap (IntMap (These Eps a)) -> Graph
epsTransitionToGraph tr = 
    buildG (IS.findMin . IM.keysSet $ tr, IS.findMax . IM.keysSet $ tr) 
        [ (i, j) 
        | (i, trI) <- IM.toList tr
        , (j, act) <- IM.toList trI
        , not . isThat $ act ]

computeClosureTransition
    :: (ChoiceSemigroup a) 
    => IntMap (IntMap (These Eps a)) -> IntSet -> IntMap a
computeClosureTransition tr =
    foldl' (IM.unionWith (<+>)) IM.empty
    . map (IM.mapMaybe justThat)
    . toList . IM.restrictKeys tr

mnfaTransitionToEnfa :: IntMap (IntMap a) -> IntMap (IntMap (These Eps a))
mnfaTransitionToEnfa = IM.map (IM.map That)

enfaToMnfa :: ChoiceSemigroup a => EpsNFA a -> MagicNFA a
enfaToMnfa (ENFA i t f) =
    let toClosures = computeClosures t
        allClosures = toList . Set.fromList . toList $ toClosures
        nStates = length allClosures
        closureToNewState = Map.fromList (zip allClosures [0..nStates])
        oldToNew = IM.map (closureToNewState Map.!) toClosures
        newT = IM.fromList $ 
            zip [0..nStates] . map (IM.mapKeysWith (<+>) (oldToNew IM.!) 
            . computeClosureTransition t)
            $ allClosures
     in MNFA
            (oldToNew IM.! i)
            newT
            (IS.fromList 
                . map (closureToNewState Map.!) 
                . filter (not . IS.null . IS.intersection f) 
                $ allClosures)

mnfaToEnfa :: MagicNFA a -> EpsNFA a
mnfaToEnfa (MNFA i t f) = ENFA i (mnfaTransitionToEnfa t) f

instance ChoiceSemigroup a => Semigroup (MagicNFA a) where
    a <> b = enfaToMnfa (mnfaToEnfa a <> mnfaToEnfa b)

instance ChoiceSemigroup a => Monoid (MagicNFA a) where
    mempty = MNFA 0 IM.empty (IS.singleton 0)

instance ChoiceSemigroup a => ChoiceSemigroup (MagicNFA a) where
    a <+> b = enfaToMnfa (mnfaToEnfa a <+> mnfaToEnfa b)

instance (ChoiceSemigroup a, ParallelSemigroup a) =>  ParallelSemigroup (MagicNFA a) where
    p  <||> q = productWith (<||>) p q 

instance (ChoiceSemigroup a, OrderedSemigroup a) =>  OrderedSemigroup (MagicNFA a) where
    p  <.> q = productWith (<.>) p q 

instance (ChoiceSemigroup a) => MonoidStar (MagicNFA a) where
    star (MNFA i t f) = enfaToMnfa $
        let ni = i + 1
            nf = shiftFinalUp 1 f
            nt = mnfaTransitionToEnfa (shiftTransitionUp 1 t)
                    `unionTransition` IM.singleton 0 (IM.singleton ni (This Eps))
                    `unionTransition` IM.fromSet (const $ IM.singleton 0 (This Eps)) nf
         in ENFA 0 nt (IS.singleton 0)

instance Pointed MagicNFA where
    point x = MNFA 0 (IM.fromList [(0, IM.singleton 1 x), (1, IM.empty)]) (IS.singleton 1)

newtype HyperAction a = HyperAction (Set a)
    deriving newtype (Foldable, Pointed)

instance Ord a => ChoiceSemigroup (HyperAction a) where
    (HyperAction a) <+> (HyperAction b) = HyperAction (a <> b)

instance (Ord a, OrderedSemigroup a) => OrderedSemigroup (HyperAction a) where
    (HyperAction xs) <.> (HyperAction ys) = HyperAction $ 
        Set.fromList [x <.> y | x <- toList xs, y <- toList ys ]

newtype HyperMagicNFA a = HyperMagicNFA (MagicNFA (HyperAction a))
    deriving newtype (ParallelSemigroup, Monoid, ChoiceSemigroup, OrderedSemigroup, MonoidStar)

instance (Ord a, ParallelSemigroup a) => ParallelSemigroup (HyperAction a) where
    (HyperAction xs) <||> (HyperAction ys) = HyperAction $ 
        Set.fromList [x <||> y | x <- toList xs, y <- toList ys ]

instance Pointed HyperMagicNFA where
    point = HyperMagicNFA . point . point

instance Ord a => Semigroup (HyperMagicNFA a) where
    (HyperMagicNFA a) <> (HyperMagicNFA b) = 
        HyperMagicNFA $ enfaToMnfa (mnfaToEnfa a <> mnfaToEnfa b)

instance Show a => Show (HyperMagicNFA a) where
    show (HyperMagicNFA x) = 
        concatMap showState $ IM.toList (mnfaTransition x)
      where
        showState (s, sTr) = 
            showStateId x s
            <> ":\n"
            <> unlines (map showTransition $ concatMap unfoldHyperAction  $ IM.toList sTr)
        unfoldHyperAction (k, ha) = map (k,) $ toList ha

stepFinalLeft :: (Int -> Int -> Int) -> Int -> IntSet -> IntSet -> IntMap (IntMap (These Eps a))
stepFinalLeft encode lFinal lF rS = IM.fromList
    [ (encode lf r, IM.singleton (encode lFinal r) (This Eps))
    | lf <- IS.toList lF, r <- IS.toList rS]

stepFinalRight :: (Int -> Int -> Int) -> IntSet -> Int -> IntSet -> IntMap (IntMap (These Eps a))
stepFinalRight encode lS rFinal rF = stepFinalLeft (flip encode) rFinal rF lS

productWith :: ChoiceSemigroup a => (a -> a -> a) -> MagicNFA a -> MagicNFA a -> MagicNFA a
productWith f (MNFA aI aT aF) (MNFA bI bT bF) =
    let aSize = IM.size aT
        aFinal = IM.size aT
        bFinal = IM.size bT
        encode x y = x + y * (aSize + 1) -- accounting for final
        bothT = productWithTransition f encode aT bT
        aFinalT = stepFinalLeft encode aFinal aF (IM.keysSet bT)
        bFinalT = stepFinalRight encode (IM.keysSet aT) bFinal bF
        onlyA = IM.mapKeysMonotonic (`encode` bFinal)
            . IM.map (IM.mapKeysMonotonic (`encode` bFinal)) $ aT
        onlyB = IM.mapKeysMonotonic (aFinal `encode`)
            . IM.map (IM.mapKeysMonotonic (aFinal `encode`)) $ bT
        newAF = IS.map (`encode` bFinal) aF
        newBF = IS.map (aFinal `encode`) bF
     in enfaToMnfa $ ENFA
            (encode aI bI)
            (mnfaTransitionToEnfa bothT
                `unionTransition` aFinalT
                `unionTransition` bFinalT
                `unionTransition` mnfaTransitionToEnfa onlyA
                `unionTransition` mnfaTransitionToEnfa onlyB)
            (newAF <>newBF)

productWithTransitionTo
    :: (a -> a -> a) -> (Int -> Int -> Int) -> IntMap a -> IntMap a -> IntMap a
productWithTransitionTo f encode atr btr = IM.fromList
    [ (encode aTo bTo, f aAct bAct )
    | (aTo, aAct) <- IM.toList atr
    , (bTo, bAct) <- IM.toList btr]

productWithTransition
    :: (a -> a -> a) -> (Int -> Int -> Int) 
    -> IntMap (IntMap a) -> IntMap (IntMap a) -> IntMap (IntMap a)
productWithTransition f encode aT bT = IM.fromList
    [ (encode aFrom bFrom, productWithTransitionTo f encode aTo bTo)
    | (aFrom, aTo) <- IM.toList aT
    , (bFrom, bTo) <- IM.toList bT
    ]


