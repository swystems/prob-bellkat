{-# LANGUAGE OverloadedStrings #-}
module AutomataSpec (spec) where

import           Data.String                (IsString)
import           Data.Pointed
import           Data.These
import           Data.These.Combinators

import BellKAT.Utils.Automata.Transitions
import BellKAT.Utils.Automata.EpsNFA 
import BellKAT.Utils.Automata.NFA 
import BellKAT.Definitions.Structures.Basic

import Test.Hspec 

newtype TestAction = TA String deriving newtype (IsString, Show, Eq, Semigroup)

instance ChoiceSemigroup TestAction where
  (<+>) = (<>)

ab :: EpsNFA TestAction
ab = ENFA 0 
    (tsFromList 
        [ (0,[(That "a", 1)])
        , (1, [(This Eps, 2)])
        , (2, [(That "b", 3)])
        , (3, [])])
    (statesFromList [3])

abC :: TransitionSystem ()
abC = tsFromList [(0, [((), 0)]), (1, [((), 1), ((), 2)]), (2, [((), 2)]), (3, [((), 3)])]

abM :: MagicNFA TestAction
abM = MNFA 0 
    (tsFromList 
        [(0,[("a", 1)]), (1, [("b", 3)]), (2, [("b", 3)]), (3,[])])
    (statesFromList [3])

aOrB :: EpsNFA TestAction
aOrB = ENFA 0 
    (tsFromList 
        [ (0,[(This Eps, 1), (This Eps, 3)])
        , (1, [(That "a", 2)])
        , (2, [])
        , (3, [(That "b", 4)])
        , (4, [])])
    (statesFromList [2, 4])

aOrbM :: MagicNFA TestAction
aOrbM = MNFA 0 
    (tsFromList 
        [ (0,[("a", 2), ("b", 4)])
        , (1, [("a", 2)])
        , (2, [])
        , (3,[("b", 4)])
        , (4,[])
        ])
    (statesFromList [2, 4])

spec :: Spec
spec = do
    describe "EpsNFA" $ do
        it "Correctly combines with <>" $ 
            (point "a" <> point "b") `shouldBe` ab
        it "Correctly combines with <+>" $ 
            (point "a" <+> point "b") `shouldBe` aOrB

    describe "computeClojure" $ do
        it "Correctly comptues for a <> b" $ 
            computeClosure ((() <$) . filterTS (not . isThat) . enfaTransition $ ab) `shouldBe` abC

    describe "computeClosureTransition" $ do
        it "Correctly comptues for [0] for a <> b" $
            let nonEps = mapMaybeTS justThere . enfaTransition $ ab
             in computeClosureTransition nonEps (statesFromList [0]) 
                    `shouldBe` transitionsFromList [("a", 1)]

    describe "EpsNFA to NFA" $ do
        it "Correctly presents combination with <>" $ 
            enfaToMnfa (point "a" <> point "b") `shouldBe` abM
        it "Correctly presents combination with <+>" $ 
            enfaToMnfa (point "a" <+> point "b") `shouldBe` aOrbM
