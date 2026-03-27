{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Test.Hspec
import Data.Either (isRight, fromLeft)

import BellKAT.Parser (parseSurfacePolicy)
import BellKAT.QuantumPrelude (QuantumTag) -- Assuming QuantumTag is the relevant tag type for P5_3_pompili
import Text.Megaparsec (errorBundlePretty)

spec :: Spec
spec = do
  describe "BellKAT.Parser" $ do
    it "successfully parses P5_3_pompili example policy" $ do
      let policyString = "let oneAttempt = (repeat 450 (if A /~ C then gen(A ~ C) else one) <||> repeat 450 (if B /~ C then gen(B ~ C) else one)) <> swap (C @ A ~ B) in whileN 1 (A /~ C || B /~ C) do oneAttempt"
      let parsed = parseSurfacePolicy @QuantumTag policyString
      case parsed of
        Right _ -> pure ()
        Left err -> expectationFailure $ "Parsing failed:\n" <> errorBundlePretty err
