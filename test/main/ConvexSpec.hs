{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module ConvexSpec where

import Control.Subcategory.Pointed

import BellKAT.Definitions.Core
import BellKAT.Utils.Distribution
import BellKAT.Utils.Convex

import Test.Hspec

spec :: Spec
spec = do
    describe "CD" $ do
        it "correctly computes minkowsky sum" $ do
            let mu0 = cpure [] :: SD BellPairs
            let mu' = [([], 1/2), (["A" :~: "B"], 1/10)] :: SD BellPairs
            -- let mu1 = pure ["A" :~: "B"] :: SD BellPairs
            let mu2 = cpure ["A" :~: "C", "B" :~: "C"] :: SD BellPairs
            let mu'' = [([], 1/2), (["A" :~: "B"], 2/10), (["A" :~: "C", "B" :~: "C"], 2/10)]
            let c1 = [mu0, mu'] :: CSD BellPairs
            let c2 = [mu0, mu2, mu'']
            combine [(c1, 7/10), (c2, 3/10)] `shouldBe` 
                [ cpure []
                , [([], 17/20), (["A" :~: "B"], 6/100), (["A" :~: "C", "B" :~: "C"], 6/100)]
                , [([], 1/2),   (["A" :~: "B"], 13/100), (["A" :~: "C", "B" :~: "C"], 6/100)]
                , [([], 35/100),(["A" :~: "B"], 7/100) , (["A" :~: "C", "B" :~: "C"], 3/10)]
                , [([], 7/10), (["A" :~: "C", "B" :~: "C"], 3/10)]
                , [([], 13/20), (["A" :~: "B"], 7/100)]
                ]


