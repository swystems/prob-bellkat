{-# LANGUAGE OverloadedStrings #-}
module BellKAT.Prelude.Common (
    KatMode(..),
    KatCLIOpts(..),
    katCLIOptsParser,
    runKatParser,
) where

import qualified Options.Applicative as OA

-- | Common mode for KAT tools
data KatMode = KMRun | KMTrace | KMProbability | KMAutomaton
  deriving stock (Show, Eq)

-- | Common CLI options for KAT tools
data KatCLIOpts = KCO
    { kcoJSON :: Bool
    , kcoMode :: KatMode
    }

-- Helper function to create subparser commands
commandParser :: String -> String -> KatMode -> OA.Mod OA.CommandFields KatMode
commandParser cmdName progDesc mode =
    OA.command cmdName (OA.info (pure mode) (OA.progDesc progDesc))

-- | Parser for KAT CLI options
katCLIOptsParser :: OA.Parser KatCLIOpts
katCLIOptsParser = KCO
    <$> OA.flag False True (OA.long "json" <> OA.help "Generate JSON")
    <*> OA.subparser (
            commandParser "run" "Run the protocol" KMRun
            <>
            commandParser "execution-trace" "Run the protocol" KMTrace
            <>
            commandParser "automaton" "Run the protocol" KMAutomaton
            <>
            commandParser "probability" "Compute event probability" KMProbability
        )

-- | Runs the CLI parser with a given program description
runKatParser :: String -> IO KatCLIOpts
runKatParser progDesc =
    OA.execParser $ OA.info (katCLIOptsParser OA.<**> OA.helper) (OA.fullDesc <> OA.progDesc progDesc)
