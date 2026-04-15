{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module QuantumSpec where

import Test.Hspec
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import Data.Monoid (Sum(..))
import Data.Ratio ((%))
import Data.List (isInfixOf)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified BellKAT.Utils.Multiset as Mset
import BellKAT.Definitions.Core
import BellKAT.Bundles.Core (runNonLoggedPipeline)
import BellKAT.Bundles.OpBased (probStarPolicyQMDPPipeline', probStarPolicyWMDPPipeline')
import BellKAT.Utils.MDP
  ( MDP(..)
  , StepCost(..)
  , fromGenerator
  , minimizeStateSystem
  , parallelCompose
  )
import BellKAT.Implementations.MDPProbability
  ( StaticBellPairs
  , holdsStaticTest
  , toStaticBellPairs
  )
import BellKAT.Implementations.MDPWerner
  ( WernerBellPairs
  )
import BellKAT.Implementations.MDPExtremal
  ( CoverageStatus(..)
  , ExtremalQuery(..)
  , computeExtremalReachability
  , erCoverageStatus
  , erInitialState
  , erMaxTable
  , erMinTable
  , erResolvedBudget
  , renderExtremalResult
  )
import BellKAT.Implementations.QuantumOps
import BellKAT.Implementations.ProbabilisticQuantumOps (StateKind(..))
import BellKAT.Utils.Convex (getGenerators)
import BellKAT.Utils.Distribution as D
import GHC.Exts (toList)
import BellKAT.Utils.Multiset (labelledMempty)
import BellKAT.Implementations.Configuration (ExecutionParams (..), NetworkCapacity, applyExecutionParams)
import BellKAT.Utils.Automata.Transitions.Functorial (StateSystem(..))
import BellKAT.QuantumPrelude
  ( ProbabilisticActionConfiguration(..)
  , QBKATPolicy, QBKATTag, QBKATRuntimeTag, QBKATTest
  , NetworkState
  , create, distill, trans, swap, ucreate, while, (/~?), (~~?), (<||>)
  )

-- | Helper to build a labelled multiset of tagged bell pairs with a given clock
buildState :: [TaggedBellPair QuantumTag] -> MaxClock -> Mset.LabelledMultiset MaxClock (TaggedBellPair QuantumTag)
buildState bps clk = Mset.fromList bps Mset.@ clk

expectSingleton :: (Show a, Eq a) => D.D' a -> a
expectSingleton d = case D.toListD d of
  [(x,p)] | p == 1 -> x
  xs -> error $ "expected singleton distribution with prob 1, got " <> show xs

expectInitialGenerator :: (Ord s, Show s, Show p, Eq p) => StateSystem (MDP p) s -> s -> D p ((Int, s), StepCost)
expectInitialGenerator ss st =
  case IM.lookup (fst (ssInitial ss)) (ssTransitions ss) >>= Map.lookup st of
    Nothing -> error $ "expected initial state in system, got " <> show st
    Just mdp ->
      case getGenerators (unMDP mdp) of
        [gen] -> gen
        gens -> error $ "expected exactly one generator, got " <> show (length gens)

expectOutcome :: (Eq s, Show s) => s -> D Double ((Int, s), StepCost) -> (Double, Int)
expectOutcome target gen =
  case [ (prob, getSum (getStepCost cost))
       | (((_, st), cost), prob) <- D.toListD gen
       , st == target
       ] of
    [result] -> result
    xs -> error $ "expected one outcome for " <> show target <> ", got " <> show xs

shouldApproxBe :: Double -> Double -> Expectation
shouldApproxBe actual expected =
  abs (actual - expected) `shouldSatisfy` (< 1e-12)

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
          outB = "A" ~ "B"
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
          outB = "A" ~ "B"
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
          ep = EP { epNetworkCapacity = Nothing
                  , epFilter          = \tbp clk' -> isFresh tbp clk' (Just 0) 
               }
          Mset.LMS (filteredSet, _) = applyExecutionParams ep initial
          wernerParams = [ w | TaggedBellPair _ (QuantumTag _ w) <- toList filteredSet ]
      -- Expect only the two fresh pairs to remain
      length wernerParams `shouldBe` 2
      wernerParams `shouldBe` replicate 2 freshW
      -- (redundant equality check emphasizing uniformity)
      all (== freshW) wernerParams `shouldBe` True

  describe "shared MDP core" $ do
    it "uses the maximum cost when composing parallel generators" $ do
      let lhs =
            fromGenerator
              [ ((["A" ~ "B"] :: StaticBellPairs, StepCost (Sum 1)), 1 :: Rational) ]
          rhs =
            fromGenerator
              [ ((["B" ~ "C"] :: StaticBellPairs, StepCost (Sum 3)), 1 :: Rational) ]
          [gen] = getGenerators . unMDP $ parallelCompose lhs rhs

      D.toListD gen `shouldBe`
        [ ((["A" ~ "B", "B" ~ "C"] :: StaticBellPairs, StepCost (Sum 3)), 1 :: Rational) ]

    it "minimizes deterministic zero-cost hops without changing the reachable behavior" $ do
      let terminal = ["A" ~ "C"] :: StaticBellPairs
          promoted = ["A" ~ "B"] :: StaticBellPairs
          ss = SS
                { ssInitial = (0, mempty)
                , ssTransitions = IM.fromList
                    [ (0, Map.fromList
                        [ (mempty, fromGenerator [(((1, promoted), StepCost (Sum 0)), 1 :: Rational)])
                        ])
                    , (1, Map.fromList
                        [ (promoted, fromGenerator [(((2, terminal), StepCost (Sum 2)), 1 :: Rational)])
                        ])
                    ]
                }
          minimized = minimizeStateSystem ss

      ssInitial minimized `shouldBe` (1, promoted)
      IM.lookup 0 (ssTransitions minimized) `shouldBe` Nothing
      fmap (Map.keys) (IM.lookup 1 (ssTransitions minimized)) `shouldBe` Just [promoted]

  describe "static MDP" $ do
    it "builds a finite Pa MDP with per-distribution costs and elides empty labels" $ do
      let pac = PAC
            { pacTransmitProbability = [(("C","A"), 8/10), (("C","B"), 7/10)]
            , pacCreateProbability   = [("C", 9/10)]
            , pacSwapProbability     = [("C", 6/10)]
            , pacUCreateProbability  = []
            , pacCreateWerner        = [("C", 958/1000)]
            , pacUCreateWerner       = []
            , pacCoherenceTime       = [("A",100),("B",100),("C",100)]
            , pacDistances           = [(("A","C"),1), (("B","C"),2), (("A","B"),3)]
            }
          capacity :: NetworkCapacity QBKATTag
          capacity = ["C" ~ "C", "C" ~ "C", "A" ~ "C", "B" ~ "C", "A" ~ "B"]
          ep :: ExecutionParams QBKATTag QBKATRuntimeTag MaxClock
          ep = EP { epNetworkCapacity = Just capacity
                  , epFilter = \_ _ -> True
                  }
          pol :: QBKATPolicy
          pol = while ("A" /~? "B")
                ( (create "C" <||> create "C")
                  <>
                  (trans "C" ("A", "C") <||> trans "C" ("B", "C"))
                  <>
                  swap "C" ("A", "B")
                )
          rendered = show $
            runNonLoggedPipeline
              (probStarPolicyQMDPPipeline' @Rational pac ep (toStaticBellPairs (mempty :: NetworkState)))
              pol

      rendered `shouldSatisfy` not . ("@()" `isInfixOf`)
      rendered `shouldSatisfy`
        isInfixOf "^0:\n  ^⦃⦄: ⦅ (0,⦃⦄)×《1 % 100, 1》+(1,⦃C~C⦄)×《9 % 50, 1》+(1,⦃C~C,C~C⦄)×《81 % 100, 1》 ⦆"
      rendered `shouldSatisfy`
        isInfixOf "  ⦃C~C,C~C⦄: ⦅ (0,⦃⦄)×《3 % 50, 2》+(0,⦃A~C⦄)×《6 % 25, 2》+(0,⦃B~C⦄)×《7 % 50, 2》+(2,⦃A~C,B~C⦄)×《14 % 25, 2》 ⦆"
      rendered `shouldSatisfy`
        isInfixOf "  ⦃A~C,B~C⦄: ⦅ (0,⦃⦄)×《2 % 5, 2》+(0,⦃A~B⦄)×《3 % 5, 2》 ⦆"

    it "computes extremal CDFs for the Pa MDP" $ do
      let pac = PAC
            { pacTransmitProbability = [(("C","A"), 8/10), (("C","B"), 7/10)]
            , pacCreateProbability   = [("C", 9/10)]
            , pacSwapProbability     = [("C", 6/10)]
            , pacUCreateProbability  = []
            , pacCreateWerner        = [("C", 958/1000)]
            , pacUCreateWerner       = []
            , pacCoherenceTime       = [("A",100),("B",100),("C",100)]
            , pacDistances           = [(("A","C"),1), (("B","C"),2), (("A","B"),3)]
            }
          capacity :: NetworkCapacity QBKATTag
          capacity = ["C" ~ "C", "C" ~ "C", "A" ~ "C", "B" ~ "C", "A" ~ "B"]
          ep :: ExecutionParams QBKATTag QBKATRuntimeTag MaxClock
          ep = EP { epNetworkCapacity = Just capacity
                  , epFilter = \_ _ -> True
                  }
          pol :: QBKATPolicy
          pol = while ("A" /~? "B")
                ( (create "C" <||> create "C")
                  <>
                  (trans "C" ("A", "C") <||> trans "C" ("B", "C"))
                  <>
                  swap "C" ("A", "B")
                )
          ev :: QBKATTest
          ev = "A" ~~? "B"
          mdp =
            runNonLoggedPipeline
              (probStarPolicyQMDPPipeline' @Rational pac ep (toStaticBellPairs (mempty :: NetworkState)))
              pol
          Right resultBudget =
            computeExtremalReachability (holdsStaticTest ev) (ExtremalBudget 10) mdp
          Just cMin10 =
            Map.lookup (erInitialState resultBudget) (erMinTable resultBudget) >>= IM.lookup 10
          Just cMax10 =
            Map.lookup (erInitialState resultBudget) (erMaxTable resultBudget) >>= IM.lookup 10
          Right resultCoverage =
            computeExtremalReachability (holdsStaticTest ev) (ExtremalCoverage 0.9) mdp
          Just cMin24 =
            Map.lookup (erInitialState resultCoverage) (erMinTable resultCoverage) >>= IM.lookup 24
          Just cMin25 =
            Map.lookup (erInitialState resultCoverage) (erMinTable resultCoverage) >>= IM.lookup 25
          renderedExtremal = renderExtremalResult resultBudget
          Just (A.Object jsonExtremal) = A.decode (A.encode resultBudget)
          Just (A.Object jsonSeries) = AKM.lookup "series" jsonExtremal

      erResolvedBudget resultBudget `shouldBe` 10
      cMin10 `shouldBe` 32945609380761 % 62500000000000
      cMax10 `shouldBe` 36225649041561 % 62500000000000
      renderedExtremal `shouldSatisfy` isInfixOf "t   pmf_min[t]"
      jsonExtremal `shouldSatisfy` AKM.member "series"
      jsonSeries `shouldSatisfy` AKM.member "cdf_min"
      jsonSeries `shouldSatisfy` AKM.member "cdf_max"
      jsonSeries `shouldSatisfy` not . AKM.member "pmf_min"
      jsonSeries `shouldSatisfy` not . AKM.member "t"

      erResolvedBudget resultCoverage `shouldBe` 25
      cMin24 `shouldSatisfy` (< 9 % 10)
      cMin25 `shouldSatisfy` (>= 9 % 10)
      erCoverageStatus resultCoverage `shouldBe`
        Just (CoverageReached { coverageTarget = 0.9, coverageBudget = 25, coverageValue = cMin25 })

  describe "Werner MDP" $ do
    it "splits parallel creation into pure and mixed Bell-pair states" $ do
      let pac = PAC
            { pacTransmitProbability = []
            , pacCreateProbability   = [("C", 9/10)]
            , pacSwapProbability     = []
            , pacUCreateProbability  = []
            , pacCreateWerner        = [("C", 9/10)]
            , pacUCreateWerner       = []
            , pacCoherenceTime       = [("C",100)]
            , pacDistances           = []
            }
          ep :: ExecutionParams () () ()
          ep = EP { epNetworkCapacity = Nothing, epFilter = \_ _ -> True }
          pol :: QBKATPolicy
          pol = create "C" <||> create "C"
          ss =
            runNonLoggedPipeline
              (probStarPolicyWMDPPipeline' @Double pac ep (mempty :: WernerBellPairs))
              pol
          gen = expectInitialGenerator ss (mempty :: WernerBellPairs)
          purePair = [TaggedBellPair ("C" ~ "C") Pure] :: WernerBellPairs
          mixedPair = [TaggedBellPair ("C" ~ "C") Mixed] :: WernerBellPairs
          twoPure = [TaggedBellPair ("C" ~ "C") Pure, TaggedBellPair ("C" ~ "C") Pure] :: WernerBellPairs
          pureMixed = [TaggedBellPair ("C" ~ "C") Pure, TaggedBellPair ("C" ~ "C") Mixed] :: WernerBellPairs
          twoMixed = [TaggedBellPair ("C" ~ "C") Mixed, TaggedBellPair ("C" ~ "C") Mixed] :: WernerBellPairs
          (pEmpty, cEmpty) = expectOutcome (mempty :: WernerBellPairs) gen
          (pPure, cPure) = expectOutcome purePair gen
          (pMixed, cMixed) = expectOutcome mixedPair gen
          (pTwoPure, cTwoPure) = expectOutcome twoPure gen
          (pPureMixed, cPureMixed) = expectOutcome pureMixed gen
          (pTwoMixed, cTwoMixed) = expectOutcome twoMixed gen

      pEmpty `shouldApproxBe` 0.01
      pPure `shouldApproxBe` 0.162
      pMixed `shouldApproxBe` 0.018
      pTwoPure `shouldApproxBe` 0.6561
      pPureMixed `shouldApproxBe` 0.1458
      pTwoMixed `shouldApproxBe` 0.0081
      all (== 1) ([cEmpty, cPure, cMixed, cTwoPure, cPureMixed, cTwoMixed] :: [Int]) `shouldBe` True

    it "uses the whole round cost for combined Pd-style actions" $ do
      let pac = PAC
            { pacTransmitProbability = []
            , pacCreateProbability   = []
            , pacSwapProbability     = [("C", 1/2)]
            , pacUCreateProbability  = [(("A", "B"), 1/10000)]
            , pacCreateWerner        = []
            , pacUCreateWerner       = [(("A", "B"), 9/10)]
            , pacCoherenceTime       = [("A",100),("B",100),("C",100),("D",100)]
            , pacDistances           = [(("A", "B"), 2), (("B", "C"), 1), (("C", "D"), 3)]
            }
          ep :: ExecutionParams () () ()
          ep = EP { epNetworkCapacity = Nothing, epFilter = \_ _ -> True }
          pol :: QBKATPolicy
          pol = ucreate ("A", "B") <||> swap "C" ("B", "D")
          initial = [TaggedBellPair ("B" ~ "C") Pure, TaggedBellPair ("C" ~ "D") Pure] :: WernerBellPairs
          ss =
            runNonLoggedPipeline
              (probStarPolicyWMDPPipeline' @Double pac ep initial)
              pol
          gen = expectInitialGenerator ss initial
          onlyABPure = [TaggedBellPair ("A" ~ "B") Pure] :: WernerBellPairs
          onlyABMixed = [TaggedBellPair ("A" ~ "B") Mixed] :: WernerBellPairs
          onlyBDPure = [TaggedBellPair ("B" ~ "D") Pure] :: WernerBellPairs
          (pABPure, cABPure) = expectOutcome onlyABPure gen
          (pABMixed, cABMixed) = expectOutcome onlyABMixed gen
          (pBDPure, cBDPure) = expectOutcome onlyBDPure gen
          pGe = 1 / 10000 :: Double
          qGe = 1 - pGe
          pSw = 1 / 2 :: Double
          qSw = 1 - pSw
          w0 = 9 / 10 :: Double
          cAB1 = exp (-1 / 100 - 1 / 100)
          cBD3 = exp (-3 / 100 - 3 / 100)

      pABPure `shouldApproxBe` (pGe * w0 * cAB1 * qSw)
      pABMixed `shouldApproxBe` (pGe * ((1 - w0) + w0 * (1 - cAB1)) * qSw)
      pBDPure `shouldApproxBe` (qGe * pSw * cBD3)
      all (== 3) ([cABPure, cABMixed, cBDPure] :: [Int]) `shouldBe` True
      all (\((_, cost), _) -> getSum (getStepCost cost) == 3) (D.toListD gen) `shouldBe` True

    it "applies the mixed-plus-pure distillation table" $ do
      let pac = PAC
            { pacTransmitProbability = []
            , pacCreateProbability   = []
            , pacSwapProbability     = []
            , pacUCreateProbability  = []
            , pacCreateWerner        = []
            , pacUCreateWerner       = []
            , pacCoherenceTime       = [("A",100),("B",100)]
            , pacDistances           = [(("A", "B"), 1)]
            }
          ep :: ExecutionParams () () ()
          ep = EP { epNetworkCapacity = Nothing, epFilter = \_ _ -> True }
          pol :: QBKATPolicy
          pol = distill ("A", "B")
          initial = [TaggedBellPair ("A" ~ "B") Mixed, TaggedBellPair ("A" ~ "B") Pure] :: WernerBellPairs
          ss =
            runNonLoggedPipeline
              (probStarPolicyWMDPPipeline' @Double pac ep initial)
              pol
          gen = expectInitialGenerator ss initial
          pureAB = [TaggedBellPair ("A" ~ "B") Pure] :: WernerBellPairs
          mixedAB = [TaggedBellPair ("A" ~ "B") Mixed] :: WernerBellPairs
          (pEmpty, cEmpty) = expectOutcome (mempty :: WernerBellPairs) gen
          (pPure, cPure) = expectOutcome pureAB gen
          (pMixed, cMixed) = expectOutcome mixedAB gen

      pEmpty `shouldApproxBe` 0.5
      pPure `shouldApproxBe` (1 / 6)
      pMixed `shouldApproxBe` (1 / 3)
      all (== 1) ([cEmpty, cPure, cMixed] :: [Int]) `shouldBe` True

main :: IO ()
main = hspec spec
