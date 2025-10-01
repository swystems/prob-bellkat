{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module QuantumSpec where

import Test.Hspec
import qualified BellKAT.Utils.Multiset as Mset
import BellKAT.Definitions.Core
import BellKAT.Implementations.QuantumOps
import BellKAT.Utils.Distribution as D
import GHC.Exts (toList)

expectSingleton :: (Show a, Eq a) => D.D' a -> a
expectSingleton d = case D.toListD d of
  [(x,p)] | p == 1 -> x
  xs -> error $ "expected singleton distribution with prob 1, got " <> show xs

spec :: Spec
spec = do
  describe "swapBPs" $ do
    it "computes Werner parameter as w1*w2 * exp(-|dt|/t_coh)" $ do
      let w0 = 0.8 :: Double
          t1 = 0
          t2 = 2
          p = 1
          -- two pairs with same fidelity w0=0.8 at different times (to have decay)
          bpIn1 = TaggedBellPair ("A" ~ "R1") (QuantumTag t1 w0)
          bpIn2 = TaggedBellPair ("R2" ~ "B") (QuantumTag t2 w0)
          input = Mset.fromList [bpIn1, bpIn2]
          outTemplate = TaggedBellPair ("A" ~ "B") (QuantumTag 0 0) -- tag ignored by swapBPs
          out = swapBPs p input outTemplate
          [ (resSet,_)] = D.toListD out
          [TaggedBellPair _ (QuantumTag tOut wOut)] = toList resSet
          expected = (w0 * w0) * exp (-fromIntegral (abs (t1 - t2)) / fromIntegral tCoherence)
      abs (wOut - expected) `shouldSatisfy` (< 1e-12)
      tOut `shouldBe` max t1 t2 + 1
    it "fails consuming inputs when probability=0" $ do
      let bpIn1 = TaggedBellPair ("A" ~ "R1") (QuantumTag 0 0.8)
          bpIn2 = TaggedBellPair ("R2" ~ "B") (QuantumTag 0 0.8)
          input = Mset.fromList [bpIn1, bpIn2]
          outTemplate = TaggedBellPair ("A" ~ "B") (QuantumTag 0 0)
          dist = swapBPs 0 input outTemplate
      -- Should yield only empty multiset with prob 1
      D.toListD dist `shouldBe` [(mempty,1)]
  describe "timestamp advancement" $ do
    it "create then transmit increases timestamp by one and decays" $ do
      let p = 1
          outA = TaggedBellPair ("A" ~ "A") (QuantumTag 0 0) -- template for create
          afterCreate = expectSingleton (createBP p mempty outA)
          [TaggedBellPair _ (QuantumTag tC wC)] = toList afterCreate
          outB = TaggedBellPair ("A" ~ "B") (QuantumTag 0 0)
          afterTx = expectSingleton (transmitBP p afterCreate outB)
          [TaggedBellPair _ (QuantumTag tT wT)] = toList afterTx
      tC `shouldBe` 0
      tT `shouldBe` 1
      wT `shouldSatisfy` (< wC + 1e-12) -- strictly decays or stays <= due to exp