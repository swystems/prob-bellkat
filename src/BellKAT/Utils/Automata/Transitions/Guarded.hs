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
    ) where

import           Data.Bifunctor
import           Data.Maybe

import qualified GHC.Exts                   (IsList, Item, fromList, toList)

import qualified Data.IntSet                  as IS
import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM

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
    productWithStates _ _ _ _ = error "cannot `productWithStates` terminating Next"

instance HasNext Next where
    setDoneToStep a s Done = Step a s
    setDoneToStep _ _ x = x

-- | Must guarantee disjointness of options
newtype GuardedTransitions t a = 
    GTr { gTransitionsToList :: [(t, Next a)] } deriving newtype (Monoid, Eq) -- TODO Eq needs canonicity
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

instance DecidableBoolean t => Semigroup (GuardedTransitions t a) where
    (GTr xs) <> (GTr ys) = gTransitionsFromList $ xs <> ys

instance DecidableBoolean t => GHC.Exts.IsList (GuardedTransitions t a) where
    type Item (GuardedTransitions t a) = (t, Next a)
    toList = gTransitionsToList
    fromList = gTransitionsFromList

instance HasNext (GuardedTransitions t) where
    setDoneToStep a i (GTr tr) = GTr $ fmap (second $ setDoneToStep a i) tr

instance DecidableBoolean t => CanProductWithStates (GuardedTransitions t) where
    productWithStates f encode aTr bTr = gTransitionsFromList $
        [ (ta &&* tb, productWithStates f encode na nb)
        | (ta, na) <- gTransitionsToList aTr
        , (tb, nb) <- gTransitionsToList bTr
        ]

class NextContainer t where
    filterNext :: (Next a -> Bool) -> t a -> t a

instance NextContainer (GuardedTransitions t) where
    filterNext f = GTr . filter (f . snd) . gTransitionsToList

mapTests :: DecidableBoolean t => (t -> t) -> GuardedTransitions t a -> GuardedTransitions t a
mapTests f = gTransitionsFromList . map (first f) . gTransitionsToList 

gTransitionsFromList :: DecidableBoolean t => [(t, Next a)] -> GuardedTransitions t a
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
showGuardedTransitionsWith f = unlines . map f . gTransitionsToList

showGuardedTransition :: (Show t, Show a) => (t, Next a) -> String
showGuardedTransition (t, Done) = "[" <> show t <> "]" <> "-> $"
showGuardedTransition (t, Step act j) = 
    "[" <> show t <> "]" <> "-( " <> show act <> " )-> " <> show j

checkDisjoint :: DecidableBoolean t => [t] -> Either String ()
checkDisjoint [] = pure ()
checkDisjoint (x:xs)
    | all (isFalse . (&&* x)) xs = checkDisjoint xs
    | otherwise = Left "Non disjoint"

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
    toListOfTransitions = IM.toList . unGTS
    fromTransitions i ts = GTS $
        IM.singleton i ts <> IM.fromSet (const gTransitionsEmpty) (states ts)
    loopStates  = GTS . IM.fromSet (gTransitionsSingleton true . Step ())
    (GTS ts) ! k = ts IM.! k 

instance HasNext (GuardedTransitionSystem t) where
    setDoneToStep a i (GTS ts) = GTS $ 
        fmap (setDoneToStep a i) ts <> IM.singleton i gTransitionsEmpty

instance Functor (GuardedTransitionSystem t) where
    fmap f = GTS . fmap (fmap f) . unGTS

instance DecidableBoolean t => Semigroup (GuardedTransitionSystem t a) where
    (GTS xs) <> (GTS ys) = GTS $ IM.unionWith (<>) xs ys

instance DecidableBoolean t => Monoid (GuardedTransitionSystem t a) where
    mempty = GTS IM.empty

instance DecidableBoolean t => CanProductWithStates (GuardedTransitionSystem t) where
    productWithStates f encode aT bT = GTS $ IM.fromList 
        [ (encode aFrom bFrom, productWithStates f encode aTo bTo)
        | (aFrom, aTo) <- IM.toList . unGTS $ aT
        , (bFrom, bTo) <- IM.toList . unGTS $ bT
        ]
instance NextContainer (GuardedTransitionSystem t) where
    filterNext f = GTS . IM.map (filterNext f) . unGTS

gtsFromList :: DecidableBoolean t => [(State, [(t, Next a)])] -> GuardedTransitionSystem t a
gtsFromList = GTS . IM.fromList . map (second gTransitionsFromList)
