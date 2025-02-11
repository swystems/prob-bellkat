{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module GuardedAutomataSpec (spec) where

import           GHC.Bits
import           Data.Word
import           Data.String                (IsString)
import           Data.Pointed
import           Numeric (showIntAtBase)
import           Data.Char (intToDigit)
import           Data.Foldable

import           Data.Boolean

import BellKAT.Utils.Automata.Guarded
import BellKAT.Utils.Automata.GuardedEps 
import BellKAT.Utils.Automata.Eps
import BellKAT.Utils.Automata.Transitions
import BellKAT.Utils.Automata.Transitions.Guarded
import BellKAT.Definitions.Structures.Basic

import Test.Hspec 

newtype TestAction = TA String deriving newtype (IsString, Show, Eq, Semigroup)

instance ChoiceSemigroup TestAction where
  (<+>) = (<>)

instance ParallelSemigroup TestAction where
  (TA x) <||> (TA y) = TA (x <> "|" <> y)

newtype Test5 = T5 Word32 deriving newtype (Eq, Bits)

test5FromPredicate :: (Word -> Bool) -> Test5
test5FromPredicate f = foldl' (.|.) zeroBits $ map (setBit zeroBits . fromIntegral) $ filter f [0 .. 2 ^ (5 :: Word) - 1]

test5A :: Test5
test5A = test5FromPredicate (`testBit` 0)

test5B :: Test5
test5B = test5FromPredicate (`testBit` 1)

instance Show Test5 where
    showsPrec _ (T5 x) = showIntAtBase 2 intToDigit x

instance Boolean Test5 where
    true = complement zeroBits
    false = zeroBits
    notB = complement
    (&&*) = (.&.)
    (||*) = (.|.)

instance DecidableBoolean Test5 where
    isFalse = (== false)

ab :: GuardedEpsFA Test5 TestAction
ab = GEFA 0 
    (gtsFromList 
        [ (0,[(true, Step (Right "a") 1)])
        , (1, [(true, Step (Left Eps) 2)])
        , (2, [(true, Step (Right "b") 3)])
        , (3, [(true, Done)])
        ])

aePost :: GuardedEpsFA Test5 TestAction
aePost = GEFA 0 
    (gtsFromList 
        [ (0,[(true, Step (Right "a") 1)])
        , (1, [(test5A, Done)])
        ])

bePre :: GuardedEpsFA Test5 TestAction
bePre = GEFA 0 
    (gtsFromList 
        [ (0,[(test5B, Step (Right "b") 1)])
        , (1, [(true, Done)])
        ])

aePostBePre :: GuardedEpsFA Test5 TestAction
aePostBePre = GEFA 0 
    (gtsFromList 
        [ (0,[(true, Step (Right "a") 1)])
        , (1, [(test5A, Step (Left Eps) 2)])
        , (2, [(test5B, Step (Right "b") 3)])
        , (3, [(true, Done)])
        ])

aPost :: GuardedFA Test5 TestAction
aPost = GFA 0 
    (gtsFromList 
        [ (0,[(true, Step "a" 1)])
        , (1, [(test5A, Done)])
        ])

bPre :: GuardedFA Test5 TestAction
bPre = GFA 0 
    (gtsFromList 
        [ (0,[(test5B, Step "b" 1)])
        , (1, [(true, Done)])
        ])

aPostBPre :: GuardedFA Test5 TestAction
aPostBPre = GFA 0 
    (gtsFromList 
        [ (0,[(true, Step "a" 1)])
        , (1, [(test5A &&* test5B, Step "b" 2)])
        , (2, [(true, Done)])
        ])

spec :: Spec
spec = do
    describe "Guarded Transitions" $ do
        it "Correctly does productWithStates" $ do
            let simpleTS = gtsFromList [(0, [(true, Step () 1)])]
            let simpleTS' = gtsFromList [(0, [(true, Step () 2)])]
            productWithStates (const id) (+) simpleTS simpleTS 
                `shouldBe` 
                (simpleTS' :: GuardedTransitionSystem Test5 ())
    describe "GuardedEpsFA" $ do
        it "Correctly combines with <> without tests" $ 
            (point "a" <> point "b") `shouldBe` ab
        it "Correctly combines with <> with tests" $
            (aePost <> bePre) `shouldBe` aePostBePre
    describe "GuardedFA"$ do 
        it "Correctly combines with <> with tests" $
            (aPost <> bPre) `shouldBe` aPostBPre
        it "Correctly combines `mempty <||> mempty`" $ do
            mempty <||> mempty `shouldBe` (mempty :: GuardedFA Test5 TestAction)
        it "Correctly cmoputes `a <||> mempty`" $ do
            point "a" <||> mempty `shouldBe` (point "a" :: GuardedFA Test5 TestAction)
        it "Correctly computes `mempty <||> a`" $ do
            mempty <||> point "a" `shouldBe` (point "a" :: GuardedFA Test5 TestAction)
