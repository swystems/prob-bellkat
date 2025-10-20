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
import BellKAT.Utils.Multiset (labelledMempty)

expectSingleton :: (Show a, Eq a) => D.D' a -> a
expectSingleton d = case D.toListD d of
  [(x,p)] | p == 1 -> x
  xs -> error $ "expected singleton distribution with prob 1, got " <> show xs

spec :: Spec
spec = do
  describe "swapBPs" $ do
    it "computes Werner parameter with decoherence" $ do
      let w0 = 0.8 :: Double
          t1 = 0 :: TimeUnit
          t2 = 2 :: TimeUnit
          p = 1 :: Rational
          clock = MaxClock 6 -- current clock
          -- homogeneous coherence times
          tCohSwap = 10 :: TimeUnit
          tCohL1   = 10 :: TimeUnit
          tCohL2   = 10 :: TimeUnit
          -- two pairs with same fidelity w0=0.8 at different times (to have decay)
          input = Mset.LMS (
            Mset.fromList [
              TaggedBellPair ("A" ~ "R1") (QuantumTag t1 w0),
              TaggedBellPair ("R2" ~ "B") (QuantumTag t2 w0)
            ],
            clock)
          out = swapBPs p (tCohSwap, tCohL1, tCohL2) input (TaggedBellPair ("A" ~ "B") (QuantumTag 0 0)) -- tag templated
          [ (Mset.LMS (resSet, newClock),_)] = D.toListD out
          [TaggedBellPair _ (QuantumTag tOut wOut)] = toList resSet
          -- expected: w1*w2 times decay from both memories involved in each arm until swap
          -- deltas are measured against current clock
          d1 = getMaxClock clock - t1
          d2 = getMaxClock clock - t2
          decay1 = exp (- fromIntegral d1 * (1 / fromIntegral tCohSwap + 1 / fromIntegral tCohL1) )
          decay2 = exp (- fromIntegral d2 * (1 / fromIntegral tCohSwap + 1 / fromIntegral tCohL2) )
          expectedWerner = (w0 * w0) * decay1 * decay2
      abs (wOut - expectedWerner) `shouldSatisfy` (< 1e-12)
      tOut `shouldBe` getMaxClock clock + 1
      newClock `shouldBe` MaxClock (getMaxClock clock + 1)
    it "fails consuming inputs when probability=0" $ do
      let bpIn1 = TaggedBellPair ("A" ~ "R1") (QuantumTag 0 0.8)
          bpIn2 = TaggedBellPair ("R2" ~ "B") (QuantumTag 0 0.8)
          p = 0 :: Rational
          clock = MaxClock 10
          input = Mset.LMS (
            Mset.fromList [bpIn1, bpIn2],
            clock)
          dist = swapBPs p (10, 10, 10) input (TaggedBellPair ("A" ~ "B") (QuantumTag 0 0))
      -- should yield only empty multiset with prob 1
      D.toListD dist `shouldBe` [(labelledMempty (MaxClock (getMaxClock clock + 1)), 1)]
  describe "timestamp advancement" $ do
    it "create then transmit increases timestamp by one and decays" $ do
      let p = 1 :: Rational
          outA = TaggedBellPair ("A" ~ "A") (QuantumTag 0 0) -- template for create
          -- create
          afterCreate = expectSingleton (createBP p mempty outA)
          Mset.LMS (createdBps, _) = afterCreate
          [TaggedBellPair _ (QuantumTag tC wC)] = toList createdBps
          -- transmit
          outB = TaggedBellPair ("A" ~ "B") (QuantumTag 0 0)
          afterTx = expectSingleton (transmitBP p (10, 10) afterCreate outB)
          Mset.LMS (transmittedBps, _) = afterTx
          [TaggedBellPair _ (QuantumTag tT wT)] = toList transmittedBps
      tC `shouldBe` 0
      tT `shouldBe` 1
      wT `shouldSatisfy` (< wC + 1e-12) -- strictly decays or stays <= due to exp