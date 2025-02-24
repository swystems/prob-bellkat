{-# LANGUAGE OverloadedStrings #-}
module TestsSpec (spec) where

import Test.Hspec

import BellKAT.Definitions.Core
import BellKAT.Definitions.Structures.Basic
import BellKAT.Definitions.Tests

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
