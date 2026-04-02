import BellKAT.QuantumPrelude

initState :: NetworkState
initState = createNetworkState
    [ TaggedBellPair ("B" ~ "B") (QuantumTag 1 0.9)
    , TaggedBellPair ("C" ~ "D") (QuantumTag 1 0.8)
    ]
    (MaxClock 1)

p :: QBKATPolicy
p = ucreate ("A", "B") <||> swap "C" ("B", "D")

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["C" ~ "C", "C" ~ "C", "A" ~ "C", "B" ~ "C", "A" ~ "B"]

nb :: NetworkBounds QBKATTag
nb = (NetworkBounds { nbCapacity = Just networkCapacity, nbCutoff = Nothing })

actionConfig :: Int -> ProbabilisticActionConfiguration
actionConfig tCoh = PAC 
    { pacTransmitProbability = 
        []
    , pacCreateProbability = []
    , pacSwapProbability = [("C", 1/2)]
    , pacUCreateProbability = [(("A", "B"), 1/10000)]
    , pacCreateWerner = []
    , pacUCreateWerner = [(("A", "B"), 90/100)]
    , pacCoherenceTime = [("A", tCoh), ("B", tCoh), ("C", tCoh), ("D", tCoh)]
    , pacDistances = [(("A", "B"), 2), (("B", "C"), 1), (("C", "D"), 3)]
    }

main :: IO ()
main = 
    let ev = "A" ~~? "B"
        tCoh = 100
    in qbkatMainD (actionConfig tCoh) nb ev p initState