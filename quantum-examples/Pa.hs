import BellKAT.QuantumPrelude

p :: QBKATPolicy
p = (create "C" <||> create "C")
    <>
    (trans "C" ("A", "C") <||> trans "C" ("B", "C"))
    <>
    swap "C" ("A", "B")

actionConfig :: Double -> Int -> ProbabilisticActionConfiguration
actionConfig w0 tCoh = PAC
    { pacTransmitProbability =
        [(("C", "A"), 8/10)
        ,(("C", "B"), 7/10)
        ]
    , pacCreateProbability = [("C", 9/10)]
    , pacSwapProbability = [("C", 6/10)]
    , pacUCreateProbability = []
    , pacCreateWerner = [("C", w0)]
    , pacUCreateWerner = []
    , pacCoherenceTime = [("A", tCoh), ("B", tCoh), ("C", tCoh)]
    }

main :: IO ()
main =
    let ev = "A" ~~? "B"
        w0 = 958/1000
        tCoh = 100
    in qbkatMain (actionConfig w0 tCoh) Nothing ev p mempty