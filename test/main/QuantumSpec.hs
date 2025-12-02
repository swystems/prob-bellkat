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
import BellKAT.Implementations.ProbAtomicOneStepQuantum (ExecutionParams (..), applyExecutionParams)

-- | Helper to build a labelled multiset of tagged bell pairs with a given clock
buildState :: [TaggedBellPair QuantumTag] -> MaxClock -> Mset.LabelledMultiset MaxClock (TaggedBellPair QuantumTag)
buildState bps clk = Mset.fromList bps Mset.@ clk

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
          tCohL = 10 :: TimeUnit
          tCohL1   = 10 :: TimeUnit
          tCohL2   = 10 :: TimeUnit
          distances = (1, 1)
          -- two pairs with same fidelity w0=0.8 at different times (to have decay)
          input = Mset.LMS (
            Mset.fromList [
              TaggedBellPair ("A" ~ "R1") (QuantumTag t1 w0),
              TaggedBellPair ("R2" ~ "B") (QuantumTag t2 w0)
            ],
            clock)
          out = swapBPs p (tCohL, tCohL1, tCohL2) distances input (TaggedBellPair ("A" ~ "B") (QuantumTag 0 0)) -- tag templated, unit distances
          [ (Mset.LMS (resSet, newClock),_)] = D.toListD out
          [TaggedBellPair _ (QuantumTag tOut wOut)] = toList resSet
          -- expected: w1*w2 times decay from both memories involved in each arm until swap
          -- deltas are measured against current clock
          decay1 = exp (- fromIntegral (getMaxClock clock - t1) * (1 / fromIntegral tCohL + 1 / fromIntegral tCohL1) )
          decay2 = exp (- fromIntegral (getMaxClock clock - t2) * (1 / fromIntegral tCohL + 1 / fromIntegral tCohL2) )
          decay3 = exp (- fromIntegral (max (fst distances) (snd distances)) * (1 / fromIntegral tCohL1 + 1 / fromIntegral tCohL2) )
          expectedWerner = (w0 * w0) * decay1 * decay2 * decay3
      abs (wOut - expectedWerner) `shouldSatisfy` (< 1e-12)
      tOut `shouldBe` getMaxClock clock + 1
      newClock `shouldBe` MaxClock (getMaxClock clock + 1)

    it "fails (and consumes inputs)" $ do
      let bpIn1 = TaggedBellPair ("A" ~ "R1") (QuantumTag 0 0.8)
          bpIn2 = TaggedBellPair ("R2" ~ "B") (QuantumTag 0 0.8)
          p = 0 :: Rational
          clock = MaxClock 10
          input = Mset.LMS (
            Mset.fromList [bpIn1, bpIn2],
            clock)
          dist = swapBPs p (10, 10, 10) (1, 1) input (TaggedBellPair ("A" ~ "B") (QuantumTag 0 0))
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
          afterTx = expectSingleton (transmitBP p (10, 10) 1 afterCreate outB)
          Mset.LMS (transmittedBps, _) = afterTx
          [TaggedBellPair _ (QuantumTag tT wT)] = toList transmittedBps
      tC `shouldBe` 0
      tT `shouldBe` 1
      wT `shouldSatisfy` (< wC + 1e-12) -- strictly decays or stays <= due to exp
  -- TODO: add test when two actions run in sequence / parallel

  describe "distance effects" $ do
    it "transmitBP advances timestamp and clock by distance d" $ do
      let p = 1 :: Rational
          outA = TaggedBellPair ("A" ~ "A") (QuantumTag 0 0)
          initialClock = MaxClock 6
          afterCreate = expectSingleton (createBP p (labelledMempty initialClock) outA) -- create (but at time clock)
          outB = TaggedBellPair ("A" ~ "B") (QuantumTag 0 0)
          d = 5 :: SpaceUnit -- distance to transmit
          afterTx = expectSingleton (transmitBP p (10, 10) d afterCreate outB) -- transmit
          Mset.LMS (transmittedBps, newClock) = afterTx
          [TaggedBellPair _ (QuantumTag tT _)] = toList transmittedBps
      tT `shouldBe` (getMaxClock initialClock + d)
      newClock `shouldBe` MaxClock (getMaxClock initialClock + d)

    it "generateBP advances timestamp and clock by distance d" $ do
      let p = 1 :: Rational
          d = 7 :: SpaceUnit
          w0 = 0.9 :: Double
          initialClock = MaxClock 6
          out = TaggedBellPair ("X" ~ "Y") (QuantumTag 1 w0)
          res = expectSingleton (generateBP p d (labelledMempty initialClock) out) -- generate (at time clock, distance d)
          Mset.LMS (bps, clk) = res
          [TaggedBellPair _ (QuantumTag tG wG)] = toList bps
      tG `shouldBe` getMaxClock initialClock + d
      wG `shouldBe` w0
      clk `shouldBe` MaxClock (getMaxClock initialClock + d)

    it "swapBPs sets timestamp to current clock + max distance" $ do
      let w0 = 0.5 :: Double
          t1 = 0 :: TimeUnit
          t2 = 0 :: TimeUnit
          p = 1 :: Rational
          clock = MaxClock 3
          tCohL = 20 :: TimeUnit
          tCohL1   = 20 :: TimeUnit
          tCohL2   = 20 :: TimeUnit
          input = Mset.LMS (
            Mset.fromList [
              TaggedBellPair ("A" ~ "R1") (QuantumTag t1 w0),
              TaggedBellPair ("R2" ~ "B") (QuantumTag t2 w0)
            ],
            clock)
          dists = (5, 2) :: (SpaceUnit, SpaceUnit) -- distances for the two input pairs
          maxDist = uncurry max dists
          out = swapBPs p (tCohL, tCohL1, tCohL2) dists input (TaggedBellPair ("A" ~ "B") (QuantumTag 0 0))
          [ (Mset.LMS (resSet, newClock),_)] = D.toListD out
          [TaggedBellPair _ (QuantumTag tOut _)] = toList resSet
      tOut `shouldBe` getMaxClock clock + maxDist
      newClock `shouldBe` MaxClock (getMaxClock clock + maxDist)

    it "distBPs sets timestamp to current clock + d and yields two outcomes" $ do
      let t1 = 0 :: TimeUnit
          t2 = 1 :: TimeUnit
          w1 = 0.7 :: Double
          w2 = 0.8 :: Double
          clock = MaxClock 4
          input = Mset.LMS (
            Mset.fromList [
              TaggedBellPair ("A" ~ "B") (QuantumTag t1 w1),
              TaggedBellPair ("A" ~ "B") (QuantumTag t2 w2)
            ],
            clock)
          d = 3 :: SpaceUnit -- distance to cover in the distill
          out = distBPs (10, 10) d input (TaggedBellPair ("A" ~ "B") ())
          outs = D.toListD out
      -- should have exactly two outcomes whose clocks equal clock + d
      length outs `shouldBe` 2
      all (\(Mset.LMS (_, c), _) -> c == MaxClock (getMaxClock clock + d)) outs `shouldBe` True
      -- one outcome should be empty (failure), one should be singleton (success)
      let sizes = map (\(Mset.LMS (s,_),_) -> length (toList s)) outs
      sum sizes `shouldBe` 1 -- exactly one singleton across outcomes

  describe "cutoff filtering" $ do
    it "with cutoff=0 keeps only age 0 pairs and they share identical Werner parameter" $ do
      let clock = MaxClock 10
          -- Two fresh pairs (age 0) created "now" with identical Werner parameter
          freshW = 0.75 :: Double
          fresh1 = TaggedBellPair ("A" ~ "B") (QuantumTag (getMaxClock clock) freshW)
          fresh2 = TaggedBellPair ("C" ~ "D") (QuantumTag (getMaxClock clock) freshW)
          -- Older pairs (different timestamp and fidelity) that should be filtered out
          old1   = TaggedBellPair ("A" ~ "C") (QuantumTag (getMaxClock clock - 1) 0.60)
          old2   = TaggedBellPair ("B" ~ "D") (QuantumTag (getMaxClock clock - 2) 0.90)
          initial = buildState [fresh1, fresh2, old1, old2] clock
          ep :: ExecutionParams () QuantumTag MaxClock
          ep = EP { networkCapacity = Nothing
            , bellPairFilter  = \tbp clk' -> isFresh tbp clk' (Just 0) }
          Mset.LMS (filteredSet, _) = applyExecutionParams ep initial
          wernerParams = [ w | TaggedBellPair _ (QuantumTag _ w) <- toList filteredSet ]
      -- Expect only the two fresh pairs to remain
      length wernerParams `shouldBe` 2
      wernerParams `shouldBe` replicate 2 freshW
      -- (redundant equality check emphasizing uniformity)
      all (== freshW) wernerParams `shouldBe` True