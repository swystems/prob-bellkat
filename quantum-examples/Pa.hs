import BellKAT.QuantumPrelude

p :: QBKATPolicy
p = while ("A" /~? "B") 
    (
        (create "C" <||> create "C")
        <>
        (
            ite ("A" /~? "C") (trans "C" ("A", "C")) mempty
            <||> 
            ite ("B" /~? "C") (trans "C" ("B", "C")) mempty
        )
        <>
        swap "C" ("A", "B")
    )

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["C" ~ "C", "C" ~ "C", "A" ~ "C", "B" ~ "C", "A" ~ "B"]

nb :: NetworkBounds QBKATTag
nb = (NetworkBounds { nbCapacity = Just networkCapacity, nbCutoff = Nothing })

actionConfig :: Double -> Int -> ProbabilisticActionConfiguration
actionConfig w0 tCoh = PAC
    { pacTransmitProbability =
        [(("C", "A"), 8/10000)
        ,(("C", "B"), 7/10000)
        ]
    , pacCreateProbability = [("C", 9/10)]
    , pacSwapProbability = [("C", 5/10)]
    , pacUCreateProbability = []
    , pacCreateWerner = [("C", w0)]
    , pacUCreateWerner = []
    , pacCoherenceTime = [("A", tCoh), ("B", tCoh), ("C", tCoh)]
    , pacDistances = [ (("A", "C"), 1)
                     , (("B", "C"), 1)
                     , (("A", "B"), 2)
                     ]
    }

main :: IO ()
main =
    let ev = "A" =~? "B"
        w0 = 958/1000
        tCoh = 14000
    in qbkatMainD (actionConfig w0 tCoh) nb ev p mempty