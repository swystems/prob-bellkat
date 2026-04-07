{-# LANGUAGE OverloadedStrings #-}
module BellKAT.Prelude.Common (
    KatCLIOpts,
    katCLIOptsParser,
    RunPipelines(..),
    runKatParser,
    main,
    mainWithOpts
) where

import qualified Options.Applicative as OA
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as A


import BellKAT.Utils.Convex
import BellKAT.Bundles.Core
import BellKAT.Utils.Distribution

-- | Common mode for KAT tools
data KatMode = KMRun | KMTrace | KMProbability | KMAutomaton
  deriving stock (Show, Eq)

-- | Common CLI options for KAT tools
data KatCLIOpts = KCO
    { kcoJSON :: Bool
    , kcoMode :: KatMode
    } deriving stock (Show)

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
runKatParser :: String -> OA.Parser a -> IO a
runKatParser progDesc p =
    OA.execParser $ OA.info (p OA.<**> OA.helper) (OA.fullDesc <> OA.progDesc progDesc)

data RunPipelines p policy system automaton o = RunPipelines 
    { runPipeline :: Pipeline policy (CD p o)
    , systemPipeline :: Pipeline policy system
    , automatonPipeline :: Pipeline policy automaton
    }

main 
    :: forall p. (RationalOrDouble p, A.ToJSON p, A.FromJSON p)
    => forall o. (DDom o, A.FromJSON o, A.ToJSON o)
    => forall automaton system. (Show automaton, Show system)
    => forall policy. RunPipelines p policy system automaton o 
    -> String -> policy -> (o -> Bool) -> IO ()
main rp progDesc protocol ev = do
    opts <- runKatParser progDesc katCLIOptsParser
    mainWithOpts rp opts protocol ev

mainWithOpts
    :: forall p. (RationalOrDouble p, A.ToJSON p, A.FromJSON p)
    => forall o. (DDom o, A.FromJSON o, A.ToJSON o)
    => forall automaton system. (Show automaton, Show system)
    => forall policy. RunPipelines p policy system automaton o 
    -> KatCLIOpts -> policy -> (o -> Bool) -> IO ()
mainWithOpts rp opts protocol ev =
    case kcoMode opts of
      KMRun -> do
          r <- runLoggedPipeline (runPipeline rp) protocol
          if kcoJSON opts
             then BS.putStr $ A.encode r
             else print r
      KMTrace ->
          runLoggedPipeline (systemPipeline rp) protocol >>= print
      KMAutomaton ->
          runLoggedPipeline (automatonPipeline rp) protocol >>= print
      KMProbability -> do
          mbRStored :: Maybe (CD p o) <- A.decode <$> BS.getContents
          case mbRStored of
            Nothing -> error "Couldn't parse input"
            Just rStored ->
                let probRange = computeEventProbabilityRange ev rStored
                 in if kcoJSON opts
                       then BS.putStr $ A.encode probRange
                       else print probRange
