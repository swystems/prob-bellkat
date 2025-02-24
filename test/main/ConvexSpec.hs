{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module ConvexSpec where

import Control.Subcategory.Bind
import Control.Subcategory.Pointed
import Control.Subcategory.Functor

import BellKAT.Definitions.Core
import BellKAT.Utils.Automata.Transitions.Core
import BellKAT.Utils.Distribution
import BellKAT.Utils.Convex
import Data.Map.Strict (Map)

import Test.Hspec

spec :: Spec
spec = do
    describe "CD" $ do
        it "correctly computes minkowsky sum" $ do
            let mu0 = cpure [] :: SD' BellPairs
            let mu' = [([], 1/2), (["A" ~ "B"], 1/10)] :: SD' BellPairs
            -- let mu1 = pure ["A" :~: "B"] :: SD BellPairs
            let mu'' = [([], 1/2), (["A" ~ "B"], 2/10), (["A" ~ "C", "B" ~ "C"], 2/10)]
            let mu2 = cpure ["A" ~ "C", "B" ~ "C"] :: SD' BellPairs
            let c1 = [mu0, mu'] :: CSD' BellPairs
            let c2 = [mu0, mu2, mu'']
            combine [(c1, 7/10), (c2, 3/10)] `shouldBe` 
                [ cpure []
                , [([], 17/20), (["A" ~ "B"], 6/100), (["A" ~ "C", "B" ~ "C"], 6/100)]
                , [([], 1/2),   (["A" ~ "B"], 13/100), (["A" ~ "C", "B" ~ "C"], 6/100)]
                , [([], 35/100),(["A" ~ "B"], 7/100) , (["A" ~ "C", "B" ~ "C"], 3/10)]
                , [([], 7/10), (["A" ~ "C", "B" ~ "C"], 3/10)]
                , [([], 13/20), (["A" ~ "B"], 7/100)]
                ]
        it "is associative" $ do
            let x = [[('A', 1/2), ('B', 1/2)]]
            let k2m :: Map Char (CD' Char) 
                    = [('A', cpure 'A'), ('B', [[('A', 1/2),('B', 1/2)]])]
            let k2' = (k2m !)
            let k3m :: Map Char (CD' Char) 
                    = [('B', cpure 'A'), 
                      ('A', [[('A', 2/3),('B', 1/3)]
                           ,[('A', 1/2),('B', 1/2)]])]
            let k3' = (k3m !)
            let ooo = cmap (cmap k3') (cmap k2' x)
            (cjoin . cjoin) ooo `shouldBe` (cjoin . cmap cjoin) ooo


