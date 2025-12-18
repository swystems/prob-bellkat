{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module ProtocolSnapSpec (spec) where

import Test.Hspec
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import System.Environment (lookupEnv)
import Data.Proxy (Proxy(..))
import Control.Exception (catch, IOException)

import BellKAT.QuantumPrelude
  ( ProbabilisticActionConfiguration(..)
  , NetworkBounds(..), CutoffSpec
  , QBKATPolicy, QBKATTag, QBKATRuntimeTag
  , MaxClock(..)
  , NetworkState
  , TaggedBellPair(..)
  , (~)
  , ite, ucreate, swap, create, trans, whileN, (<||>), (<.>)
  , (/~?), (&&*)
  )
import Data.Default (def)
import BellKAT.Definitions (applyProbStarPolicyQ')
import BellKAT.Implementations.Output (ListOutput)
import BellKAT.Definitions.Core (Op)
import BellKAT.Implementations.ProbAtomicOneStepQuantum (ExecutionParams(..))
import BellKAT.Implementations.QuantumOps (isFresh)

-- Output type and EP equivalent to QBKATOutput of QuantumPrelude
type TestOutput = ListOutput (TaggedBellPair (), Op QBKATRuntimeTag) MaxClock QBKATTag

buildEP :: NetworkBounds QBKATTag -> ExecutionParams QBKATTag QBKATRuntimeTag MaxClock
buildEP nb = EP { epNetworkCapacity = nbCapacity nb
                , epFilter          = \tbp clk -> isFresh tbp clk (nbCutoff nb)
             }

-- Compute JSON for a protocol run
evalProtocol :: ProbabilisticActionConfiguration
             -> NetworkBounds QBKATTag
             -> QBKATPolicy
             -> NetworkState
             -> A.Value
evalProtocol pac nb pol ns =
  let r = applyProbStarPolicyQ' @Double (Proxy :: Proxy TestOutput) pac (buildEP nb) pol ns
  in  A.toJSON r

-- If UPDATE_SNAP=1, write the file, otherwise compare results
expectMatchesSnap :: FilePath -> A.Value -> IO ()
expectMatchesSnap snapPath v = do
  mbUpdate <- lookupEnv "UPDATE_SNAP"
  case mbUpdate of
    Just "1" -> BS.writeFile snapPath (A.encode v)
    _ -> do
      exists <- BS.readFile snapPath `catch` \(_ :: IOException) -> pure BS.empty
      let isEmpty = BS.null exists
      if isEmpty
        then expectationFailure $ "Missing snap at " <> snapPath <> "; run with UPDATE_SNAP=1 to record."
        else case A.eitherDecode exists of
          Left err    -> expectationFailure $ "Invalid snap JSON: " <> err
          Right vSnap -> v `shouldBe` (vSnap :: A.Value)

spec :: Spec
spec = describe "protocol snaps" $ do
  -- Pa example (see quantum-examples/Pa.hs)
  it "Pa example: create->trans->swap (unbounded)" $ do
    let pac = PAC
          { pacTransmitProbability = [( ("C","A"), 8/10 ), ( ("C","B"), 7/10 )]
          , pacCreateProbability   = [("C", 9/10)]
          , pacSwapProbability     = [("C", 6/10)]
          , pacUCreateProbability  = []
          , pacCreateWerner        = [("C", 958/1000)]
          , pacUCreateWerner       = []
          , pacCoherenceTime       = [("A",100),("B",100),("C",100)]
          , pacDistances           = [( ("A","C"),1 ), ( ("B","C"),1 ), ( ("A","B"),2 )]
          }
        nb  = def
        pol :: QBKATPolicy
        pol = (create "C" <||> create "C")
              <> (trans "C" ("A","C") <||> trans "C" ("B","C"))
              <> swap "C" ("A","B")
    expectMatchesSnap "test/snapshots/Pa.json"
      (evalProtocol pac nb pol mempty)

  -- P5_Li_cutoff example (see quantum-examples/P5_Li_cutoff.hs)
  it "P5_Li_cutoff example: chain with cutoff, 108 iterations" $ do
    let pGen  = 1/4 :: Rational
        pSwap = 1/2 :: Rational
        w0    = 95/100 :: Double
        pac = PAC { pacTransmitProbability = []
                  , pacCreateProbability   = []
                  , pacCreateWerner        = []
                  , pacUCreateProbability  = [( ("A","B"), pGen ), ( ("B","C"), pGen )]
                  , pacSwapProbability     = [("B", pSwap)]
                  , pacUCreateWerner       = [( ("A","B"), w0 ), ( ("B","C"), w0 )]
                  , pacCoherenceTime       = [("A",100),("B",100),("C",100)]
                  , pacDistances           = [( ("A","B"),1 ), ( ("B","C"),1 ), ( ("A","C"),2 )]
                  }
        capacity = ["A" ~ "B", "A" ~ "C", "B" ~ "C"]
        nb = NetworkBounds { nbCapacity = Just capacity, nbCutoff = Just (4 :: CutoffSpec) }
        pol :: QBKATPolicy
        pol = whileN 108 ("A" /~? "C")
              ( ( ite ("A" /~? "B") (ucreate ("A","B")) mempty
                  <||>
                  ite ("B" /~? "C") (ucreate ("B","C")) mempty
                )
                <> swap "B" ("A","C")
              )
    expectMatchesSnap "test/snapshots/P5_Li_cutoff.json"
      (evalProtocol pac nb pol mempty)

  -- P5_Star example (see quantum-examples/P5_Star.hs)
  it "P5_Star example: star generations + swaps, 18 iterations" $ do
    let pac = PAC { pacTransmitProbability = []
                  , pacCreateProbability   = []
                  , pacCreateWerner        = []
                  , pacUCreateProbability  = [ (("A","H"), 1/4)
                                              , (("B","H"), 1/3)
                                              , (("C","H"), 1/3) ]
                  , pacSwapProbability     = [("H", 1/2)]
                  , pacUCreateWerner       = [ (("A","H"), 90/100)
                                              , (("B","H"), 95/100)
                                              , (("C","H"), 95/100) ]
                  , pacCoherenceTime       = [("A",100),("B",100),("C",100),("H",200)]
                  , pacDistances           = [ (("A","H"),1), (("B","H"),1), (("C","H"),1) ]
                  }
        -- Duplicate entry for "C"~"H" preserved to mirror example file exactly.
        capacity = ["A" ~ "H", "B" ~ "H", "C" ~ "H", "C" ~ "H"]
        nb = NetworkBounds { nbCapacity = Just capacity, nbCutoff = Nothing }
        pol :: QBKATPolicy
        pol = whileN 18 ("A" /~? "C" &&* "B" /~? "C")
              ( ( ite ("A" /~? "H") (ucreate ("A","H")) mempty
                  <||>
                  ite ("B" /~? "H") (ucreate ("B","H")) mempty
                  <||>
                  ite ("C" /~? "H") (ucreate ("C","H")) mempty
                )
                <> ( swap "H" ("A","C") <.> swap "H" ("B","C") )
              )
    expectMatchesSnap "test/snapshots/P5_Star.json"
      (evalProtocol pac nb pol mempty)