{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Test.Hspec

import BellKAT.Parser (parseSurfacePolicy)
import Text.Megaparsec (errorBundlePretty)
import BellKAT.DSL
import BellKAT.Definitions
import Data.Semigroup

oneAttempt :: OrderedGuardedPolicy (BoundedTest ()) (TaggedAction ())
oneAttempt = 
    let n = 450 :: Int
     in (
         stimes n (ite ("A" /~? "C") (ucreate ("A", "C")) mempty) 
            <||> 
         stimes n (ite ("B" /~? "C") (ucreate ("B", "C")) mempty)
        ) 
        <> swap "C" ("A", "B")

p :: Int -> OrderedGuardedPolicy (BoundedTest ()) (TaggedAction ())
p n = whileN n ("A" /~? "C" ||* "B" /~? "C") oneAttempt

spec :: Spec
spec = do
  describe "BellKAT.Parser" $ do
    let policyString = "let oneAttempt = ((repeat 450 if A /~ C then gen(A ~ C) else one) <||> (repeat 450 if B /~ C then gen(B ~ C) else one)) <> swap (C @ A ~ B) in whileN 1 (A /~ C || B /~ C) do oneAttempt" 
    it "successfully parses P5_3_pompili example policy" $ do
      let parsed = parseSurfacePolicy @() policyString
      case parsed of
        Right x -> fmap defaultTagged x `shouldBe` p 1
        Left err -> expectationFailure $ "Parsing failed:\n" <> errorBundlePretty err
