{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module AtomicSpec (spec) where

import Control.Subcategory.Pointed

import BellKAT.Definitions.Core
import BellKAT.Definitions.Structures
import BellKAT.Definitions.Atomic
import BellKAT.Utils.Distribution as D


import Test.Hspec

piA :: ProbabilisticAtomicAction ()
piA = createProbabilitsticAtomicAction
    (createRestrictedTest [])
    ["C" ~ "C"]
    (D.choose (4 / 5) ["A" ~ "C"] [])

piC :: ProbabilisticAtomicAction ()
piC = createProbabilitsticAtomicAction 
    (createRestrictedTest [])
    []
    (D.choose (2 / 3) ["C" ~ "C"] [])

piS :: ProbabilisticAtomicAction ()
piS = createProbabilitsticAtomicAction
    (createRestrictedTest [["C" ~ "C"]])
    []
    (cpure [])

piB :: ProbabilisticAtomicAction ()
piB = createProbabilitsticAtomicAction
    (createRestrictedTest [])
    ["C" ~ "C"]
    (D.choose (1 / 2) ["B" ~ "C"] [])


spec :: Spec
spec = do
    describe "RestrictedTest" $ do
        it "correctly does .+." $ 
            (createRestrictedTest @() [["C" ~ "C"]] .+. []) 
            `shouldBe`
            createRestrictedTest [["C" ~ "C"]]
        it "correctly does the .&&." $ 
            (createRestrictedTest @() [] .&&. createRestrictedTest [["C" ~ "C"]])
                `shouldBe` createRestrictedTest [["C" ~ "C"]]
    describe "ProbabilisticAtomicAction" $ do
        it "composes in parallel piA and piC" $ 
            (piA <||> piC) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [])
                ["C" ~ "C"]
                [ (["A" ~ "C", "C" ~ "C"], 8 / 15)
                , (["A" ~ "C"], 4 / 15)
                , (["C" ~ "C"], 2 / 15)
                , ([], 1 /15)
                ]
        it "composes in parallel piS and piC" $ 
            (piS <||> piC) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [["C" ~ "C"]])
                []
                [ (["C" ~ "C"], 2 / 3)
                , ([], 1 / 3)
                ]
        it "composes in parallel piA and piB" $ 
            (piA <||> piB) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [])
                ["C" ~ "C", "C" ~ "C"]
                [ (["A" ~ "C", "B" ~ "C"], 4 / 10)
                , (["A" ~ "C"], 4 / 10)
                , (["B" ~ "C"], 1 / 10)
                , ([], 1 / 10)
                ]
        it "composes in parallel piA and piS" $ 
            (piA <||> piS) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [["C" ~ "C", "C" ~ "C"]])
                ["C" ~ "C"]
                [ (["A" ~ "C"], 4 / 5)
                , ([], 1 / 5)
                ]
        it "composes in parallel piB and piS" $ 
            (piB <||> piS) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [["C" ~ "C", "C" ~ "C"]])
                ["C" ~ "C"]
                [ (["B" ~ "C"], 1 / 2)
                , ([], 1 / 2)
                ]
        it "composes in parallel piS and piS" $ 
            (piS <||> piS) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [["C" ~ "C"]])
                []
                (cpure [])
        it "composes in parallel piC and piC" $ 
            (piC <||> piC) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [])
                []
                [ (["C" ~ "C"], 4 / 9)
                , (["C" ~ "C", "C" ~ "C"], 4 / 9)
                , ([], 1 / 9)
                ]
