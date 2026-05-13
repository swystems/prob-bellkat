
import BellKAT.QuantumPrelude hiding (lookup)
import Data.List (intercalate, stripPrefix)
import System.Environment (getArgs, withArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)


pASAP :: QBKATPolicy
pASAP = while ("A" /~? "D")
    (
        (
            ite ("A" /~? "B" &&* "A" /~? "C") (ucreate ("A", "B")) mempty
        <||>
            ite ("B" /~? "C" &&* "B" /~? "D" &&* "A" /~? "C") (ucreate ("B", "C")) mempty
        <||>
            ite ("C" /~? "D" &&* "B" /~? "D") (ucreate ("C", "D")) mempty    
        ) 
        <||>
        (
            sswap ["B", "C"] ("A", "D")
        <.>
            (
                    swap "B" ("A", "D")
                <||>
                    swap "C" ("A", "D")
                <||>
                    swap "B" ("A", "C")
                <||>
                    swap "C" ("B", "D")
            )
        )
    )

events :: [(String, QBKATTest)]
events =
    [ ("static", "A" ~~? "D")
    , ("pure", "A" -~? "D")
    , ("mixed", "A" =~? "D")
    ]

selectEvent :: String -> Either String QBKATTest
selectEvent name =
    maybe
        (Left $ "Unknown event '" <> name <> "'. Available events: " <> availableEvents)
        Right
        (lookup name events)

availableEvents :: String
availableEvents = intercalate ", " (fmap fst events)

data ExampleArgs = ExampleArgs
    { eaEventName :: String
    , eaPrintOracle :: Bool
    , eaPGe :: Rational
    , eaTCoh :: Int
    , eaQbkatArgs :: [String]
    }

stripExampleArgs :: [String] -> Either String ExampleArgs
stripExampleArgs = go "pure" False defaultPGe defaultTCoh []
  where
    go selectedEvent printOracle pGe tCoh kept [] =
        Right ExampleArgs
            { eaEventName = selectedEvent
            , eaPrintOracle = printOracle
            , eaPGe = pGe
            , eaTCoh = tCoh
            , eaQbkatArgs = reverse kept
            }
    go _ _ _ _ _ ["--event"] = Left "Missing value for --event."
    go _ _ _ _ _ ["--p-gen"] = Left "Missing value for --p-gen."
    go _ _ _ _ _ ["--t-coh"] = Left "Missing value for --t-coh."
    go _ _ _ _ _ ["--tcoh"] = Left "Missing value for --tcoh."
    go _ _ _ _ _ ["--coherence-time"] = Left "Missing value for --coherence-time."
    go _ printOracle pGe tCoh kept ("--event" : name : rest) =
        go name printOracle pGe tCoh kept rest
    go selectedEvent printOracle _ tCoh kept ("--p-gen" : value : rest) = do
        pGe <- parsePGen value
        go selectedEvent printOracle pGe tCoh kept rest
    go selectedEvent printOracle pGe _ kept (flag : value : rest)
        | flag == "--t-coh" || flag == "--tcoh" || flag == "--coherence-time" = do
            tCoh <- parseTCoh value
            go selectedEvent printOracle pGe tCoh kept rest
    go selectedEvent _ pGe tCoh kept ("--oracle" : rest) =
        go selectedEvent True pGe tCoh kept rest
    go selectedEvent printOracle pGe tCoh kept (arg : rest)
        | Just name <- stripPrefix "--event=" arg =
            go name printOracle pGe tCoh kept rest
        | Just value <- stripPrefix "--p-gen=" arg = do
            pGe' <- parsePGen value
            go selectedEvent printOracle pGe' tCoh kept rest
        | Just value <- stripPrefix "--t-coh=" arg = do
            tCoh' <- parseTCoh value
            go selectedEvent printOracle pGe tCoh' kept rest
        | Just value <- stripPrefix "--tcoh=" arg = do
            tCoh' <- parseTCoh value
            go selectedEvent printOracle pGe tCoh' kept rest
        | Just value <- stripPrefix "--coherence-time=" arg = do
            tCoh' <- parseTCoh value
            go selectedEvent printOracle pGe tCoh' kept rest
        | otherwise =
            go selectedEvent printOracle pGe tCoh (arg : kept) rest

parsePGen :: String -> Either String Rational
parsePGen value =
    case readMaybe value :: Maybe Double of
        Just p | p > 0 && p < 1 -> Right (toRational p)
        _ -> Left $ "--p-gen must be a decimal probability in (0,1), got " <> show value

parseTCoh :: String -> Either String Int
parseTCoh value =
    case readMaybe value :: Maybe Int of
        Just t | t > 0 -> Right t
        _ -> Left $ "--t-coh must be a positive integer, got " <> show value

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "B", "B" ~ "C", "C" ~ "D", "A" ~ "C", "B" ~ "D", "A" ~ "D"]

nb :: NetworkBounds QBKATTag
nb = def
    { nbCapacity = Just networkCapacity
    , nbOperationTiming = InstantaneousOps
    }

-- | A large enough coherence time that endpoint decay is below numerical noise for
-- the validation horizons used here. Appendix D ignores endpoint memories.
noDecoherenceTime :: Int
noDecoherenceTime = 1000000000000000

defaultPGe :: Rational
defaultPGe = 1/10

defaultTCoh :: Int
defaultTCoh = 5000

paperMemoryLambda :: Int -> Double
paperMemoryLambda tCoh = exp (-1 / fromIntegral tCoh)

appendixDLambda3 :: Double -> Double -> Double
appendixDLambda3 p lambda =
    ((1 - q) ^ (3 :: Int) / ((lambda - q) ^ (2 :: Int) * (1 - lambda * q) ^ (2 :: Int)))
        *
        ( lambda ^ (2 :: Int) * (1 - lambda * q) ^ (2 :: Int) / (1 - q * lambda ^ (2 :: Int))
        + 2 * lambda * q * (1 - lambda * q) * (lambda ^ (2 :: Int) - 1) / (1 - lambda * q ^ (2 :: Int))
        + q ^ (2 :: Int) * (lambda ^ (2 :: Int) - 1) ^ (2 :: Int) / (1 - q ^ (3 :: Int))
        )
  where
    q = 1 - p

appendixDFidelity3 :: Double -> Double
appendixDFidelity3 eLambda = 3 / 4 * eLambda + 1 / 4

renderAppendixDOracle :: Rational -> Int -> String
renderAppendixDOracle pGe tCoh =
    unlines
        [ "Appendix D1 swap-ASAP oracle"
        , "Assumptions: three homogeneous links, deterministic swaps, w0 = 1, no endpoint decoherence."
        , printf "p_gen = %.15g" p
        , printf "q = 1 - p_gen = %.15g" q
        , printf "repeater t_coh = %d" tCoh
        , printf "paper lambda = exp(-1 / t_coh) = %.15g" lambda
        , printf "E[Lambda_3] = %.15g" eLambda
        , printf "E[F_3] = 3/4 E[Lambda_3] + 1/4 = %.15g" eF
        , "For qmdp validation with w0 = 1 and pSwap = 1, the total pure-event mass should converge to E[Lambda_3]."
        ]
  where
    p = fromRational pGe :: Double
    q = 1 - p
    lambda = paperMemoryLambda tCoh
    eLambda = appendixDLambda3 p lambda
    eF = appendixDFidelity3 eLambda

actionConfig :: Rational -> Int -> ProbabilisticActionConfiguration
actionConfig pGe tCoh = PAC
    { pacTransmitProbability = []
    , pacCreateProbability = []
    , pacCreateWerner = []
    , pacUCreateProbability = [(("A", "B"), pGe), (("B", "C"), pGe), (("C", "D"), pGe)]
    , pacUCreateWerner = [(("A", "B"), 1), (("B", "C"), 1), (("C", "D"), 1)]
    , pacSwapProbability = [("B", 1), ("C", 1)]
    , pacCoherenceTime =
        [ ("A", noDecoherenceTime)
        , ("B", tCoh)
        , ("C", tCoh)
        , ("D", noDecoherenceTime)
        ]
    , pacDistances =
    [ (("A", "B"), 1)
    , (("B", "C"), 1)
    , (("C", "D"), 1)
    , (("A", "C"), 2)
    , (("B", "D"), 2)
    , (("A", "D"), 3)
    ]
    }

main :: IO ()
main = do
    args <- getArgs
    exampleArgs <-
        either fail pure (stripExampleArgs args)
    let pGe = eaPGe exampleArgs
        tCoh = eaTCoh exampleArgs
        eventName = eaEventName exampleArgs
    ev <- either fail pure (selectEvent eventName)
    if eaPrintOracle exampleArgs
       then putStr (renderAppendixDOracle pGe tCoh)
       else withArgs (eaQbkatArgs exampleArgs) $
            qbkatMainD (actionConfig pGe tCoh) nb ev pASAP mempty
