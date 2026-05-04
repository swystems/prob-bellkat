import BellKAT.QuantumPrelude

p :: QBKATPolicy 
p = while ("A" /~? "C")
    (       
        (
            ite ("A" /~? "B") (ucreate ("A", "B")) mempty
        <||> 
            ite ("B" /~? "C") (ucreate ("B", "C")) mempty
        )
        <> 
            swap "B" ("A", "C")
    )

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "B", "B" ~ "C", "A" ~ "C"]

nb :: NetworkBounds QBKATTag
nb = def
    { nbCapacity = Just networkCapacity
    , nbOperationTiming = InstantaneousOps
    }

actionConfig :: Double -> Int -> ProbabilisticActionConfiguration
actionConfig w0 tCoh = PAC 
    { pacTransmitProbability = []
    , pacCreateProbability = []
    , pacCreateWerner = []
    , pacUCreateProbability = [(("A", "B"), 1/3), (("B", "C"), 1/5)]
    , pacUCreateWerner = [(("A", "B"), w0), (("B", "C"), w0)]
    , pacSwapProbability = [("B", 1/2)]
    , pacCoherenceTime = [("A", tCoh), ("B", tCoh), ("C", tCoh)]
    , pacDistances =
    [ (("A", "B"), 1)
    , (("B", "C"), 1)
    , (("A", "C"), 2)
    ]
    }

main :: IO ()
main = 
    let ev  = "A" ~~? "C"
        w0  = 8/10
        tCoh = 10
    in qbkatMainD (actionConfig w0 tCoh) nb ev p mempty
