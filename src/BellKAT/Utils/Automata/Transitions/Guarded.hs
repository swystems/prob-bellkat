{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveFunctor #-}
module BellKAT.Utils.Automata.Transitions.Guarded 
    (
    -- * Next transition
      Next(..)
    , isDone
    , isStep
    , HasNext(..)
    -- * Guarded transitions
    , GuardedTransitions
    , NextContainer(..)
    , gTransitionsSingleton
    , gTransitionsToList
    , gTransitionsFromList
    , gTransitionsITE
    , mapTests
    , showGuardedTransitionsWith
    , showGuardedTransition
    -- * Guarded transition system
    , GuardedTransitionSystem
    , singletonGts
    , singletonDoneGts
    , gtsFromList
    , loopStates
    , loopDone
    ) where

import           Data.Bifunctor
import           Data.Maybe
import           Data.List (intercalate)

import qualified GHC.Exts                   (IsList, Item, fromList, toList)

import qualified Data.IntSet                  as IS
import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM hiding ((!), (!?))

import BellKAT.Utils.Automata.Transitions
import BellKAT.Definitions.Structures.Basic

data Next a = Done | Step a Int deriving stock (Eq, Functor)

isDone :: Next a -> Bool
isDone Done = True
isDone _ = False 

isStep :: Next a -> Bool
isStep (Step _ _) = True
isStep _ = False 

class HasNext t where
    setDoneToStep :: a -> State -> t a -> t a
    setStepToDone :: t a -> t a

class HasTests f t | f -> t where
    mapTests :: (Show t, DecidableBoolean t) => (t -> t) -> f -> f

instance HasStates (Next a) where
    states Done = mempty 
    states (Step _ k) = singletonState k

    numStates Done = 0
    numStates (Step _ _) = 1

instance CanMapStates (Next a) where
    mapStates _ Done = Done
    mapStates f (Step a k) = Step a (f k)

instance CanProductWithStates Next where
    productWithStates f encode (Step xa xi) (Step ya yi) = Step (f xa ya) (encode xi yi) 
    productWithStates _ _ Done Done = Done 
    productWithStates _ _ _ _ = error "cannot `productWithStates` Step and Done"

instance HasNext Next where
    setDoneToStep a s Done = Step a s
    setDoneToStep _ _ x = x

    setStepToDone _ = Done

-- | Must guarantee disjointness of options
newtype GuardedTransitions t a = 
    GTr { gTransitionsToList :: [(t, Next a)] } deriving newtype (Monoid, Eq) -- TODO Eq needs canonicity

instance (Show t, Show a) => Show (GuardedTransitions t a) where
    show = showGuardedTransitionsWith showGuardedTransition

instance HasStates1 (GuardedTransitions t) where
    states1 = states
    numStates1 = numStates

instance HasStates (GuardedTransitions t a) where
    states = mconcat . map (states . snd) . gTransitionsToList
    numStates = IS.size . states

instance CanRestrictStates (GuardedTransitions t a) where
    restrictStates s = GTr . filter (helper . snd) . gTransitionsToList
        where 
            helper Done = True
            helper (Step _ k) = k `statesMember` s

instance LikeTransitions (GuardedTransitions t) where
    toTransitionsList = mapMaybe (helper . snd) . gTransitionsToList
      where 
        helper Done = Nothing
        helper (Step a i) = Just (a, i)

instance CanMapStates (GuardedTransitions t a) where
    mapStates f = GTr . fmap (second $ mapStates f) . gTransitionsToList
    mapStatesMonotonic f = GTr . fmap (second $ mapStatesMonotonic f) . gTransitionsToList

instance Functor (GuardedTransitions t) where
    fmap f (GTr xs) = GTr $ fmap (second $ fmap f) xs

instance (Show t, DecidableBoolean t) => Semigroup (GuardedTransitions t a) where
    (GTr xs) <> (GTr ys) = gTransitionsFromList $ xs <> ys

instance (Show t, DecidableBoolean t) => GHC.Exts.IsList (GuardedTransitions t a) where
    type Item (GuardedTransitions t a) = (t, Next a)
    toList = gTransitionsToList
    fromList = gTransitionsFromList

instance HasNext (GuardedTransitions t) where
    setDoneToStep a i (GTr tr) = GTr $ fmap (second $ setDoneToStep a i) tr
    setStepToDone (GTr tr) = GTr $ fmap (second setStepToDone) tr

instance (Show t, DecidableBoolean t) => CanProductWithStates (GuardedTransitions t) where
    productWithStates f encode aTr bTr = gTransitionsFromList $
        [ (ta &&* tb, productWithStates f encode na nb)
        | (ta, na) <- gTransitionsToList aTr
        , (tb, nb) <- gTransitionsToList bTr
        ]

class NextContainer t where
    filterNext :: (Next a -> Bool) -> t a -> t a

instance NextContainer (GuardedTransitions t) where
    filterNext f = GTr . filter (f . snd) . gTransitionsToList

instance HasTests (GuardedTransitions t a) t where
    mapTests f = gTransitionsFromList . map (first f) . gTransitionsToList 

gTransitionsFromList :: (Show t, DecidableBoolean t) => [(t, Next a)] -> GuardedTransitions t a
gTransitionsFromList xs = 
    let nonEmpty = filter (not . isFalse . fst) xs
     in case checkDisjoint (map fst nonEmpty) of
          Left err -> error err
          Right () -> GTr nonEmpty

-- TODO: should check for emptyness
gTransitionsSingleton :: t -> Next a -> GuardedTransitions t a
gTransitionsSingleton t n  = GTr [(t,  n)] 

gTransitionsEmpty :: GuardedTransitions t a
gTransitionsEmpty = GTr []

-- TODO: should check for emptyness
gTransitionsITE :: Boolean t => t -> Next a -> Next a -> GuardedTransitions t a
gTransitionsITE t a b = GTr [(t, a), (notB t, b)]

showGuardedTransitionsWith :: ((t, Next a) -> String) -> GuardedTransitions t a -> String
showGuardedTransitionsWith f = intercalate "\n" . map f . gTransitionsToList

showGuardedTransition :: (Show t, Show a) => (t, Next a) -> String
showGuardedTransition (t, Done) = "[" <> show t <> "]" <> "-> $"
showGuardedTransition (t, Step act j) = 
    "[" <> show t <> "]" <> "-( " <> show act <> " )-> " <> show j

checkDisjoint :: (Show t, DecidableBoolean t) => [t] -> Either String ()
checkDisjoint [] = pure ()
checkDisjoint (x:xs) = 
    case filter (not . isFalse . (&&* x)) xs of
      [] -> checkDisjoint xs
      (y:_) -> Left $ "Non disjoint: " <> show x <> " and " <> show y

newtype GuardedTransitionSystem t a = 
    GTS { unGTS :: IntMap (GuardedTransitions t a) } deriving newtype Eq
    
instance HasStates1 (GuardedTransitionSystem t) where
    states1 = states
    numStates1 = numStates

instance HasStates (GuardedTransitionSystem t a) where
    states = IM.keysSet . unGTS
    numStates = IM.size . unGTS

instance CanRestrictStates (GuardedTransitionSystem t a) where
    restrictStates s = GTS . IM.map (restrictStates s) . (`IM.restrictKeys` s) . unGTS

instance CanMapStates (GuardedTransitionSystem t a) where
    mapStates f = GTS . IM.mapKeys f . IM.map (mapStates f)  . unGTS

singletonGts :: State -> t -> a -> State -> GuardedTransitionSystem t a
singletonGts i t a j = GTS $
    IM.singleton i (gTransitionsSingleton t $ Step a j) 
    <> (if i == j then mempty else IM.singleton j gTransitionsEmpty)

singletonDoneGts :: t -> State -> GuardedTransitionSystem t a
singletonDoneGts t i = GTS $ IM.singleton i (gTransitionsSingleton t Done)

instance Boolean t => LikeTransitionSystem (GuardedTransitionSystem t) where
    type TSTransitions (GuardedTransitionSystem t) = GuardedTransitions t
    -- toListOfTransitions = IM.toList . unGTS
    fromTransitions i ts = GTS $
        IM.singleton i ts <> IM.fromSet (const gTransitionsEmpty) (states ts)
    loopStates  = GTS . IM.fromSet (gTransitionsSingleton true . Step ())

instance StaticMap (GuardedTransitionSystem t a) where
    type Key (GuardedTransitionSystem t a) = State
    type Val (GuardedTransitionSystem t a) = GuardedTransitions t a

    lookup k (GTS ts) = ts !? k 
    size (GTS ts) = size ts
    k `member` (GTS ts) = k `member` ts

instance (Show t, DecidableBoolean t) => GHC.Exts.IsList (GuardedTransitionSystem t a) where
    type Item (GuardedTransitionSystem t a) = (State, GuardedTransitions t a)
    toList = IM.toList . unGTS
    -- TODO: construct directly
    fromList = mconcat . map (uncurry fromTransitions)

instance (Show a, Show t, DecidableBoolean t) => Show (GuardedTransitionSystem t a) where
    show x = unlines $ map showState $ toPairs x
      where
        showState (s, sTr) = 
            show s 
            <> ": "
            <> showGuardedTransitionsWith showGuardedTransition sTr

instance HasNext (GuardedTransitionSystem t) where
    setDoneToStep a i (GTS ts) = GTS $ 
        fmap (setDoneToStep a i) ts <> IM.singleton i gTransitionsEmpty
    setStepToDone = GTS . fmap setStepToDone . unGTS

instance Functor (GuardedTransitionSystem t) where
    fmap f = GTS . fmap (fmap f) . unGTS

instance (Show t, DecidableBoolean t) => Semigroup (GuardedTransitionSystem t a) where
    (GTS xs) <> (GTS ys) = GTS $ IM.unionWith (<>) xs ys

instance (Show t, DecidableBoolean t) => Monoid (GuardedTransitionSystem t a) where
    mempty = GTS IM.empty

instance (Show t, DecidableBoolean t) => CanProductWithStates (GuardedTransitionSystem t) where
    productWithStates f encode aT bT = GTS $ IM.fromList 
        [ (encode aFrom bFrom, productWithStates f encode aTo bTo)
        | (aFrom, aTo) <- IM.toList . unGTS $ aT
        , (bFrom, bTo) <- IM.toList . unGTS $ bT
        ]
instance NextContainer (GuardedTransitionSystem t) where
    filterNext f = GTS . IM.map (filterNext f) . unGTS

instance HasTests (GuardedTransitionSystem t a) t where
    mapTests f = GTS . IM.map (mapTests f) . unGTS

loopDone :: GuardedTransitionSystem t () -> GuardedTransitionSystem t ()
loopDone = GTS . IM.mapWithKey (\s -> setDoneToStep () s) . unGTS

gtsFromList :: (Show t, DecidableBoolean t) => [(State, [(t, Next a)])] -> GuardedTransitionSystem t a
gtsFromList = GTS . IM.fromList . map (second gTransitionsFromList)
