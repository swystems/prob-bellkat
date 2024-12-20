{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module AtomicSpec (spec) where

import BellKAT.Definitions.Structures
import BellKAT.DSL
import BellKAT.Definitions.Atomic

import qualified Numeric.Probability.Distribution as P

import Test.Hspec

piA :: ProbabilisticAtomicAction ()
piA = createProbabilitsticAtomicAction
    (createRestrictedTest [])
    ["C" ~ "C"]
    (P.choose (4 / 5) ["A" ~ "C"] [])

piC :: ProbabilisticAtomicAction ()
piC = createProbabilitsticAtomicAction 
    (createRestrictedTest [])
    []
    (P.choose (2 / 3) ["C" ~ "C"] [])

spec :: Spec
spec = do
    describe "ProbabilisticAtomicAction" $ do
        it "composes in parallel piA and piC" $ 
            (piA <||> piC) `shouldBe` createProbabilitsticAtomicAction 
                (createRestrictedTest [])
                ["C" ~ "C"]
                (P.fromFreqs [ (["A" ~ "C", "C" ~ "C"], 8 / 15)
                             , (["A" ~ "C"], 4 / 15)
                             , (["C" ~ "C"], 2 / 15)
                             , ([], 1 /15)
                             ])

