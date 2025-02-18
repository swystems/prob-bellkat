{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
module BellKAT.Utils.Automata.NFA
    ( MagicNFA(..)
    , HyperMagicNFA(..)
    , restrictStates
    , productWith
    , showStateId
    , enfaToMnfa
    ) where

import           Data.Pointed
import           Data.Foldable (toList)
import           Data.These
import           Data.These.Combinators       (isThat, justThere)

import           BellKAT.Utils.Automata.Transitions
import           BellKAT.Utils.Automata.EpsNFA
import           BellKAT.Utils.Automata.HyperAction
import           BellKAT.Definitions.Structures.Basic

data MagicNFA a = MNFA
    { mnfaInitial    :: Int
    , mnfaTransition :: TransitionSystem a
    , mnfaFinal      :: States
    } deriving stock (Eq)

instance HasStates (MagicNFA a) where
    states = states . mnfaTransition
    numStates = numStates . mnfaTransition

instance CanRestrictStates (MagicNFA a) where
    restrictStates s x = MNFA
        { mnfaInitial = mnfaInitial x
        , mnfaTransition = restrictStates s $ mnfaTransition x
        , mnfaFinal = restrictStates s $ mnfaFinal x
        }

instance Show a => Show (MagicNFA a) where
    show x =
        concatMap showState . toPairs . mnfaTransition $ x
      where
        showState (s, sTr) =
            showStateId x s
            <> ":\n"
            <> showTransitionsWith showTransition sTr

showStateId :: MagicNFA a -> Int -> String
showStateId x s =
    (if s == mnfaInitial x then "^" else "")
    <> show s
    <> (if statesMember s (mnfaFinal x) then "$" else "")

mnfaTransitionToEnfa :: TransitionSystem a -> TransitionSystem (These Eps a)
mnfaTransitionToEnfa = fmap That

enfaToMnfa :: ChoiceSemigroup a => EpsNFA a -> MagicNFA a
enfaToMnfa (ENFA i t f) =
    let eps = computeClosure . (() <$) . filterTS (not . isThat) $ t
        nonEps = mapMaybeTS justThere t
        newT = tsFromStates (\k -> computeClosureTransition nonEps (states $ eps ! k)) $ states t
     in MNFA
            i
            newT
            (filterStates ((/= mempty) . statesIntersection f . states . (eps !)) . states $ t)

mnfaToEnfa :: MagicNFA a -> EpsNFA a
mnfaToEnfa (MNFA i t f) = ENFA i (mnfaTransitionToEnfa t) f

instance ChoiceSemigroup a => Semigroup (MagicNFA a) where
    a <> b = enfaToMnfa (mnfaToEnfa a <> mnfaToEnfa b)

instance ChoiceSemigroup a => Monoid (MagicNFA a) where
    mempty = MNFA 0 mempty (singletonState 0)

instance ChoiceSemigroup a => ChoiceSemigroup (MagicNFA a) where
    a <+> b = enfaToMnfa (mnfaToEnfa a <+> mnfaToEnfa b)

instance (ChoiceSemigroup a, ParallelSemigroup a) =>  ParallelSemigroup (MagicNFA a) where
    p  <||> q = productWith (<||>) p q

instance (ChoiceSemigroup a, OrderedSemigroup a) =>  OrderedSemigroup (MagicNFA a) where
    p  <.> q = productWith (<.>) p q

instance (ChoiceSemigroup a) => MonoidStar (MagicNFA a) where
    star (MNFA i t f) = enfaToMnfa $
        let ni = i + 1
            nf = shiftUp 1 f
            nt = mnfaTransitionToEnfa (shiftUp 1 t)
                    <> singletonTs 0 (This Eps) ni
                    <> sendStatesInto (This Eps) 0 nf
         in ENFA 0 nt (singletonState 0)

instance Pointed MagicNFA where
    point x = MNFA 0 (singletonTs 0 x 1) (singletonState 1)

newtype HyperMagicNFA a = HyperMagicNFA (MagicNFA (HyperAction a))
    deriving newtype (ParallelSemigroup, Monoid, ChoiceSemigroup, OrderedSemigroup, MonoidStar)

instance Pointed HyperMagicNFA where
    point = HyperMagicNFA . point . point

instance Ord a => Semigroup (HyperMagicNFA a) where
    (HyperMagicNFA a) <> (HyperMagicNFA b) =
        HyperMagicNFA $ enfaToMnfa (mnfaToEnfa a <> mnfaToEnfa b)

instance Show a => Show (HyperMagicNFA a) where
    show (HyperMagicNFA x) =
        concatMap showState . toPairs . mnfaTransition $ x
      where
        showState (s, sTr) =
            showStateId x s
            <> ":\n"
            <> showTransitionsWith (unlines . map showTransition . unfoldHyperAction) sTr

        unfoldHyperAction (k, ha) = map (k,) $ toList ha

productWith :: ChoiceSemigroup a => (a -> a -> a) -> MagicNFA a -> MagicNFA a -> MagicNFA a
productWith f (MNFA aI aT aF) (MNFA bI bT bF) =
    let aSize = numStates aT
        aFinal = numStates aT
        bFinal = numStates bT
        encode x y = x + y * (aSize + 1) -- accounting for final
        bothT = productWithStates f encode aT bT
        aFinalT = stepFinalLeft encode aFinal aF (states bT)
        bFinalT = stepFinalRight encode (states aT) bFinal bF
        onlyA = mapStatesMonotonic (`encode` bFinal) aT
        onlyB = mapStatesMonotonic (aFinal `encode`) bT
        newAF = mapStates (`encode` bFinal) aF
        newBF = mapStates (aFinal `encode`) bF
     in enfaToMnfa $ ENFA
            (encode aI bI)
            (mnfaTransitionToEnfa bothT
                <> aFinalT
                <> bFinalT
                <> mnfaTransitionToEnfa onlyA
                <> mnfaTransitionToEnfa onlyB)
            (newAF <> newBF)
  where
    stepFinalLeft :: (Int -> Int -> Int) -> Int -> States -> States -> TransitionSystem (These Eps a)
    stepFinalLeft encode lFinal lF rS =
        This Eps <$ productStates encode (sendStatesInto () lFinal lF) (loopStates rS)

    stepFinalRight :: (Int -> Int -> Int) -> States -> Int -> States -> TransitionSystem (These Eps a)
    stepFinalRight encode lS rFinal rF = stepFinalLeft (flip encode) rFinal rF lS

