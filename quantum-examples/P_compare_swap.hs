import BellKAT.QuantumPrelude hiding (lookup)
import Data.List (intercalate, stripPrefix)
import System.Environment (getArgs, withArgs)

pGen :: QBKATPolicy
pGen =
            ite ("A" /~? "B" &&* "A" /~? "C") (ucreate ("A", "B")) mempty
        <||>
            ite ("B" /~? "C" &&* "B" /~? "D" &&* "A" /~? "C") (ucreate ("B", "C")) mempty
        <||>
            ite ("C" /~? "D" &&* "B" /~? "D") (ucreate ("C", "D")) mempty

pSwapASAP :: QBKATPolicy
pSwapASAP =
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

pASAP :: QBKATPolicy
pASAP = while ("A" /~? "D")
    (
        pGen 
        <> 
        pSwapASAP
        <> 
        pSwapASAP
    )

pSeq1 :: QBKATPolicy
pSeq1 = while ("A" /~? "D")
    (
        pGen
        <>
        swap "B" ("A", "C")
        <>
        swap "C" ("A", "D")
    )

pSeq2 :: QBKATPolicy
pSeq2 = while ("A" /~? "D")
    (
        pGen
        <>
        swap "C" ("B", "D")
        <>
        swap "B" ("A", "D")
    )

pSim :: QBKATPolicy
pSim = while ("A" /~? "D")
    (
        pGen
        <>
        sswap ["B", "C"] ("A", "D")
    )


pOpt :: QBKATPolicy
pOpt = while ("A" /~? "D")
    (
        pGen
        <||>
            sswap ["B", "C"] ("A", "D")
        <||>
            swap "B" ("A", "D")
        <||>
            swap "C" ("A", "D")
        <||>
            swap "B" ("A", "C")
        <||>
            idle [("A", "B"), ("B", "C")]
        <||>
            swap "C" ("B", "D")
        <||>
            idle [("B", "C"), ("C", "D")]
    )

protocols :: [(String, QBKATPolicy)]
protocols =
    [ ("asap", pASAP)
    , ("seq1", pSeq1)
    , ("seq2", pSeq2)
    , ("sim", pSim)
    , ("opt", pOpt)
    ]

events :: [(String, QBKATTest)]
events =
    [ ("static", "A" ~~? "D")
    , ("pure", "A" -~? "D")
    , ("mixed", "A" =~? "D")
    ]

selectProtocol :: String -> Either String QBKATPolicy
selectProtocol name =
    maybe
        (Left $ "Unknown protocol '" <> name <> "'. Available protocols: " <> availableProtocols)
        Right
        (lookup name protocols)

selectEvent :: String -> Either String QBKATTest
selectEvent name =
    maybe
        (Left $ "Unknown event '" <> name <> "'. Available events: " <> availableEvents)
        Right
        (lookup name events)

availableProtocols :: String
availableProtocols = intercalate ", " (fmap fst protocols)

availableEvents :: String
availableEvents = intercalate ", " (fmap fst events)

stripExampleArgs :: [String] -> Either String (String, String, [String])
stripExampleArgs = go "asap" "pure" []
  where
    go selectedProtocol selectedEvent kept [] = Right (selectedProtocol, selectedEvent, reverse kept)
    go _ _ _ ["--protocol"] = Left "Missing value for --protocol."
    go _ _ _ ["--event"] = Left "Missing value for --event."
    go _ selectedEvent kept ("--protocol" : name : rest) = go name selectedEvent kept rest
    go selectedProtocol _ kept ("--event" : name : rest) = go selectedProtocol name kept rest
    go selectedProtocol selectedEvent kept (arg : rest)
        | Just name <- stripPrefix "--protocol=" arg = go name selectedEvent kept rest
        | Just name <- stripPrefix "--event=" arg = go selectedProtocol name kept rest
        | otherwise = go selectedProtocol selectedEvent (arg : kept) rest

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "B", "B" ~ "C", "C" ~ "D", "A" ~ "C", "B" ~ "D", "A" ~ "D"]

nb :: NetworkBounds QBKATTag
nb = def { nbCapacity = Just networkCapacity }

-- Case 1: fully homogeneous, swap-asap is better
-- actionConfig :: Double -> Int -> ProbabilisticActionConfiguration
-- actionConfig w0 tCoh = PAC
--     { pacTransmitProbability = []
--     , pacCreateProbability = []
--     , pacCreateWerner = []
--     , pacUCreateProbability = [(("A", "B"), 1/20), (("B", "C"), 1/20), (("C", "D"), 1/20)]
--     , pacUCreateWerner = [(("A", "B"), w0), (("B", "C"), w0), (("C", "D"), w0)]
--     , pacSwapProbability = [("B", 1/2), ("C", 1/2)]
--     , pacCoherenceTime = [("A", tCoh), ("B", tCoh), ("C", tCoh), ("D", tCoh)]
--     , pacDistances =
--     [ (("A", "B"), 1)
--     , (("B", "C"), 1)
--     , (("C", "D"), 1)
--     , (("A", "C"), 2)
--     , (("B", "D"), 2)
--     , (("A", "D"), 3)
--     ]
--     }

-- Case 2: heterogeneous, sequential is better cause it fixes the optimal swap (start from the right swap, then the left one)
actionConfig :: Double -> Int -> ProbabilisticActionConfiguration
actionConfig w0 tCoh = PAC
    { pacTransmitProbability = []
    , pacCreateProbability = []
    , pacCreateWerner = []
    , pacUCreateProbability = [(("A", "B"), 1/20), (("B", "C"), 1/20), (("C", "D"), 1/200)]
    , pacUCreateWerner = [(("A", "B"), w0), (("B", "C"), w0), (("C", "D"), w0)]
    , pacSwapProbability = [("B", 1/2), ("C", 1/2)]
    , pacCoherenceTime = [("A", tCoh), ("B", tCoh), ("C", tCoh), ("D", tCoh)]
    , pacDistances =
    [ (("A", "B"), 1)
    , (("B", "C"), 1)
    , (("C", "D"), 1)
    , (("A", "C"), 2)
    , (("B", "D"), 2)
    , (("A", "D"), 3)
    ]
    }

-- Case 3: heterogeneous, but on the other side (start from the left swap, then the right one is better)
-- actionConfig :: Double -> Int -> ProbabilisticActionConfiguration
-- actionConfig w0 tCoh = PAC
--     { pacTransmitProbability = []
--     , pacCreateProbability = []
--     , pacCreateWerner = []
--     , pacUCreateProbability = [(("A", "B"), 1/200), (("B", "C"), 1/20), (("C", "D"), 1/20)]
--     , pacUCreateWerner = [(("A", "B"), w0), (("B", "C"), w0), (("C", "D"), w0)]
--     , pacSwapProbability = [("B", 1/2), ("C", 1/2)]
--     , pacCoherenceTime = [("A", tCoh), ("B", tCoh), ("C", tCoh), ("D", tCoh)]
--     , pacDistances =
--     [ (("A", "B"), 1)
--     , (("B", "C"), 1)
--     , (("C", "D"), 1)
--     , (("A", "C"), 2)
--     , (("B", "D"), 2)
--     , (("A", "D"), 3)
--     ]
--     }




main :: IO ()
main = do
    args <- getArgs
    (protocolName, eventName, qbkatArgs) <-
        either fail pure (stripExampleArgs args)
    protocol <- either fail pure (selectProtocol protocolName)
    ev <- either fail pure (selectEvent eventName)
    let w0  = 97/100
        tCoh = 5000
    withArgs qbkatArgs $
        qbkatMainD (actionConfig w0 tCoh) nb ev protocol mempty
