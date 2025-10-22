import BellKAT.QuantumPrelude

pG1 :: Int -> QBKATPolicy
pG1 n =
    whileN n ("A" /~? "B") (ucreate ("A", "B"))
        <||>
    whileN n ("B" /~? "C") (ucreate ("B", "C"))

pG2 :: Int -> QBKATPolicy
pG2 n =
    whileN n ("C" /~? "D") (ucreate ("C", "D"))
        <||>
    whileN n ("D" /~? "E") (ucreate ("D", "E"))

pS :: Int -> QBKATPolicy
pS n =
    whileN n ("A" /~? "C") (pG1 n <> swap "B" ("A", "C"))
        <||>
    whileN n ("C" /~? "E") (pG2 n <> swap "D" ("C", "E"))

p :: Int -> QBKATPolicy
p n = whileN n ("A" /~? "E") (pS n <> swap "C" ("A", "E"))

actionConfig :: Rational -> Double -> Rational -> Int -> ProbabilisticActionConfiguration
actionConfig pGen w0 pSwap tCoh =
    PAC
        { pacTransmitProbability = []
        , pacCreateProbability = []
        , pacCreateWerner = []
        , pacUCreateProbability =
            [ (("A", "B"), pGen)
            , (("B", "C"), pGen)
            , (("C", "D"), pGen)
            , (("D", "E"), pGen)
            ]
        , pacSwapProbability =
            [ ("B", pSwap)
            , ("C", pSwap)
            , ("D", pSwap)
            ]
        , pacUCreateWerner =
            [ (("A", "C"), w0)
            , (("B", "C"), w0)
            , (("C", "D"), w0)
            , (("D", "E"), w0)
            ]
        , pacCoherenceTime =
            [ ("A", tCoh)
            , ("B", tCoh)
            , ("C", tCoh)
            , ("D", tCoh)
            , ("E", tCoh)
        ]
        , pacDistances =
            [ (("A", "B"), 1)
            , (("B", "C"), 1)
            , (("C", "D"), 1)
            , (("D", "E"), 1)
            , (("A", "C"), 2)
            , (("B", "D"), 2)
            , (("C", "E"), 2)
            , (("A", "D"), 3)
            , (("B", "E"), 3)
            , (("A", "E"), 4)
            ]
        }

main :: IO ()
main =
    let ev     = "A" ~~? "E"
        pGen  = 26/10000
        pSwap = 75/100
        w0    = 958/1000
        tCoh  = 100
     in qbkatMainD (actionConfig pGen w0 pSwap tCoh) Nothing ev (p 2) mempty