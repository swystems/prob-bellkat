{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module PaperSpec where

import qualified Data.Set as Set
import Test.Hspec

import BellKAT.Prelude 
import BellKAT.Definitions (UTree(..), applyStarPolicyH)

p1 :: BellKATPolicy 
p1 =
    (create "C" <||> create "C" <||> create "E" <||> create "E") 
    <>
    (trans "C" ("A", "D") <||> trans "C" ("B", "D") <||> trans "E" ("E", "D") <||> trans "E" ("E", "D")) 
    <>
    (swap "D" ("A", "E") <||> swap "D" ("B", "E"))

p2 :: BellKATPolicy
p2 =
    (create "C" <||> create "C" <||> create "C" <||> create "C") 
    <>
    (trans "C" ("C", "A") <||> trans "C" ("C", "B") <||> trans "C" ("C", "D") <||> trans "C" ("C", "D") <||> create "E" <||> create "E") 
    <>
    (swap "C" ("A", "D") <||> swap "C" ("B", "D") <||> trans "E" ("E", "D") <||> trans "E" ("E", "D"))
    <> 
    (swap "D" ("A", "E") <||> swap "D" ("B", "E"))

p3 :: BellKATPolicy
p3 = 
    let 
        pd =
            (create "C" <||> create "C") <>
                (trans "C" ("A", "D") <||> trans "C" ("A", "D")) <>
                    distill ("A", "D")
        bpd =
            (test ("A" /~? "D") <.> create "C" <||> test ("A" /~? "D") <.> create "C") <>
                (trans "C" ("A", "D") <||> trans "C" ("A", "D")) <>
                    distill ("A", "D")
        pd' =
            (create "E" <||> create "E") <>
                (trans "E" ("E", "D") <||> trans "E" ("E", "D")) <>
                    distill ("E", "D")
        bpd' =
            (test ("E" /~? "D") <.> create "E" <||> test ("E" /~? "D") <.> create "E") <>
                (trans "E" ("E", "D") <||> trans "E" ("E", "D")) <>
                    distill ("E", "D")
        pad = pd <> star bpd <> test ("A" ~~? "D") 
        ped = pd' <> star bpd' <> test ("E" ~~? "D")
     in (pad <||> ped) <> swap "D" ("A", "E")

spec :: Spec
spec = do
    describe "P1" $ do
        it "should generate correct history" $
            (Set.elems . applyStarPolicyH p1 $ []) 
            `shouldBe` 
            [[ Node ("A" ~ "E") 
                [ Node ("A" ~ "D") [Node ("C" ~ "C") []] 
                , Node ("E" ~ "D") [Node ("E" ~ "E") []]
                ]
             , Node ("B" ~ "E") 
                [ Node ("B" ~ "D") [Node ("C" ~ "C") []]
                , Node ("E" ~ "D") [Node ("E" ~ "E") []]
                ]
            ]]
    describe "P2" $ do
        it "should generate correct history" $
            (Set.elems . applyStarPolicyH p2 $ []) 
            `shouldBe` 
            [[ Node ("A" ~ "E") 
                [ Node ("A" ~ "D") 
                    [ Node ("C" ~ "A") [Node ("C" ~ "C") []]
                    , Node ("C" ~ "D") [Node ("C" ~ "C") []]
                    ]
                , Node ("E" ~ "D") [Node ("E" ~ "E") []]
                ]
             , Node ("B" ~ "E") 
                [ Node ("B" ~ "D") 
                    [ Node ("C" ~ "B") [Node ("C" ~ "C") []]
                    , Node ("C" ~ "D") [Node ("C" ~ "C") []] 
                    ]
                , Node ("E" ~ "D") [Node ("E" ~ "E") []]
                ]
            ]]
    describe "P3 [LONG]" $ do
        it "always returns A~E" $
            arePoliciesEquivalent [[]] p3 (p3 <> test ("A" ~~? "E")) `shouldBe` True
        it "not always creates A~C" $
            arePoliciesEquivalent [[]] p3 (p3 <> test ("A" ~~? "C")) `shouldBe` False
        it "uses more than 1 qubit at A" $
            isPolicyValid [[]] (memoryBounds [("A", 1)]) p3 `shouldBe` False
        it "uses more than 3 qubit at D" $
            isPolicyValid [[]] (memoryBounds [("D", 3)]) p3 `shouldBe` False
        it "uses no more than 2 qubits at A and no more than 4 qubits at D" $
            isPolicyValid [[]] (memoryBounds [("A", 2), ("D", 4)]) p3 `shouldBe` True

