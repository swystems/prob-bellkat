{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module TestsSpec (spec) where

import Test.Hspec

import BellKAT.DSL
import BellKAT.Definitions.Core
import BellKAT.Definitions.Structures.Basic
import BellKAT.Definitions.Tests
import BellKAT.Implementations.MDPProbability
import BellKAT.Implementations.MDPWerner
import BellKAT.Implementations.ProbabilisticQuantumOps (StateKind(..))

spec :: Spec
spec = do
    describe "BoundedTest.notB" $ do
        it "correctly sends true to false" $
            notB (true :: BoundedTest ()) `shouldBe` false
        it "correctly sends false to true" $
            notB (false :: BoundedTest ()) `shouldBe` true
        it "correctly cancels one thingy" $
            notB (boundedTestSingleton ("A" ~ "B" @ ()) (rangeGreater 0)) 
                `shouldBe`
                boundedTestSingleton ("A" ~ "B" @ ()) (rangeNotGreater 0)
    describe "BoundedTests.&&*" $ do
        it "correctly ands incompatibles" $ 
            notB (boundedTestSingleton ("A" ~ "B" @ ()) (rangeGreater 0)) 
                &&*
                boundedTestSingleton ("A" ~ "B" @ ()) (rangeGreater 0)
                    `shouldBe` false
        it "correctly ands other incompatibles" $ 
            notB (boundedTestSingleton ("A" ~ "B" @ ()) (rangeNotGreater 0)) 
                &&*
                boundedTestSingleton ("A" ~ "B" @ ()) (rangeNotGreater 0)
                    `shouldBe` false
    describe "KindedTest" $ do
        let staticAB :: StaticBellPairs
            staticAB = ["A" ~ "B"]

            pureAB :: WernerBellPairs
            pureAB = [TaggedBellPair ("A" ~ "B") Pure]

            mixedAB :: WernerBellPairs
            mixedAB = [TaggedBellPair ("A" ~ "B") Mixed]

        it "collapses pure selectors to static on time-only states" $
            holdsStaticTest ("A" -~? "B" :: KindedTest ()) staticAB `shouldBe` True

        it "collapses mixed selectors to static on time-only states" $
            holdsStaticTest ("A" =~? "B" :: KindedTest ()) staticAB `shouldBe` True

        it "matches pure Werner states exactly" $ do
            holdsWernerTest ("A" -~? "B" :: KindedTest ()) pureAB `shouldBe` True
            holdsWernerTest ("A" -~? "B" :: KindedTest ()) mixedAB `shouldBe` False

        it "matches mixed Werner states exactly" $ do
            holdsWernerTest ("A" =~? "B" :: KindedTest ()) mixedAB `shouldBe` True
            holdsWernerTest ("A" =~? "B" :: KindedTest ()) pureAB `shouldBe` False

        it "keeps Werner guards static even for pure and mixed selectors" $ do
            holdsWernerGuardTest ("A" -~? "B" :: KindedTest ()) mixedAB `shouldBe` True
            holdsWernerGuardTest ("A" =~? "B" :: KindedTest ()) pureAB `shouldBe` True
