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

-- TODO: check if it differs because it is R1: Gen, R2: Swap, R3: Gen, R4: Swap, ...
-- instead of being as the other protocols R1: Gen, R2: Swap, R3: Swap, R4: Gen, ...
pASAP :: QBKATPolicy
pASAP = while ("A" /~? "D")
    (
        pGen <>
        (
            sswap ["B", "C"] ("A", "D")
        <||>
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


protocols :: [(String, QBKATPolicy)]
protocols =
    [ ("asap", pASAP)
    , ("seq1", pSeq1)
    , ("seq2", pSeq2)
    , ("sim", pSim)
    ]

selectProtocol :: String -> Either String QBKATPolicy
selectProtocol name =
    maybe
        (Left $ "Unknown protocol '" <> name <> "'. Available protocols: " <> availableProtocols)
        Right
        (lookup name protocols)

availableProtocols :: String
availableProtocols = intercalate ", " (fmap fst protocols)

stripProtocolArgs :: [String] -> Either String (String, [String])
stripProtocolArgs = go "asap" []
  where
    go selected kept [] = Right (selected, reverse kept)
    go _ _ ["--protocol"] = Left "Missing value for --protocol."
    go _ kept ("--protocol" : name : rest) = go name kept rest
    go selected kept (arg : rest)
        | Just name <- stripPrefix "--protocol=" arg = go name kept rest
        | otherwise = go selected (arg : kept) rest

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "B", "B" ~ "C", "C" ~ "D", "A" ~ "C", "B" ~ "D", "A" ~ "D"]

nb :: NetworkBounds QBKATTag
nb = (NetworkBounds { nbCapacity = Just networkCapacity, nbCutoff = Nothing })

actionConfig :: Double -> Int -> ProbabilisticActionConfiguration
actionConfig w0 tCoh = PAC
    { pacTransmitProbability = []
    , pacCreateProbability = []
    , pacCreateWerner = []
    , pacUCreateProbability = [(("A", "B"), 1/20), (("B", "C"), 1/20), (("C", "D"), 1/50)]
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

main :: IO ()
main = do
    args <- getArgs
    (protocolName, qbkatArgs) <-
        either fail pure (stripProtocolArgs args)
    protocol <- either fail pure (selectProtocol protocolName)
    let ev  = "A" -~? "D"
        w0  = 8/10
        tCoh = 10
    withArgs qbkatArgs $
        qbkatMainD (actionConfig w0 tCoh) nb ev protocol mempty
