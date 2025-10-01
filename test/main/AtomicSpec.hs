{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module AtomicSpec (spec) where

import BellKAT.Definitions.Core
import BellKAT.Definitions.Structures
import BellKAT.Definitions.Atomic

import Test.Hspec

piA :: ProbabilisticAtomicAction ()
piA = createProbabilitsticAtomicAction
    (createRestrictedTest [])
    [ (["C" ~ "C"], FTransmit (4/5) ("A" ~ "C")) ]

piC :: ProbabilisticAtomicAction ()
piC = createProbabilitsticAtomicAction 
    (createRestrictedTest [])
    [ ([], FCreate (2/3) ("C" ~ "C")) ]

piS :: ProbabilisticAtomicAction ()
piS = createProbabilitsticAtomicAction
    (createRestrictedTest [["C" ~ "C"]])
    [ ([], FSkip) ]

piB :: ProbabilisticAtomicAction ()
piB = createProbabilitsticAtomicAction
    (createRestrictedTest [])
    [ (["C" ~ "C"], FTransmit (1/2) ("B" ~ "C")) ]


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
                [ (["C" ~ "C"], FTransmit (4/5) ("A" ~ "C"))
                , ([], FCreate (2/3) ("C" ~ "C"))
                ]
        it "composes in parallel piS and piC" $ 
            (piS <||> piC) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [["C" ~ "C"]])
                [ ([], FSkip)
                , ([], FCreate (2/3) ("C" ~ "C"))
                ]
        it "composes in parallel piA and piB" $ 
            (piA <||> piB) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [])
                [ (["C" ~ "C"], FTransmit (4/5) ("A" ~ "C"))
                , (["C" ~ "C"], FTransmit (1/2) ("B" ~ "C"))
                ]
        it "composes in parallel piA and piS" $ 
            (piA <||> piS) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [["C" ~ "C", "C" ~ "C"]])
                [ (["C" ~ "C"], FTransmit (4/5) ("A" ~ "C")) 
                , ([], FSkip)
                ]
        it "composes in parallel piB and piS" $ 
            (piB <||> piS) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [["C" ~ "C", "C" ~ "C"]])
                [ (["C" ~ "C"], FTransmit (1/2) ("B" ~ "C"))
                , ([], FSkip)
                ]
        it "composes in parallel piS and piS" $ 
            (piS <||> piS) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [["C" ~ "C"]])
                [ ([], FSkip)
                , ([], FSkip)
                ]
        it "composes in parallel piC and piC" $ 
            (piC <||> piC) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [])
                [ ([], FCreate (2/3) ("C" ~ "C"))
                , ([], FCreate (2/3) ("C" ~ "C"))
                ]
