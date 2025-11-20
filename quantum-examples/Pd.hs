import BellKAT.QuantumPrelude

initState :: NetworkState
initState = createNetworkState
    [ TaggedBellPair ("A" ~ "B") (QuantumTag 1 0.9)
    , TaggedBellPair ("A" ~ "B") (QuantumTag 1 0.8)
    , TaggedBellPair ("A" ~ "B") (QuantumTag 1 0.7)
    , TaggedBellPair ("A" ~ "B") (QuantumTag 1 0.6)
    ]
    (MaxClock 1)

p :: QBKATPolicy
p = distill ("A", "B")

actionConfig :: Int -> ProbabilisticActionConfiguration
actionConfig tCoh = PAC 
    { pacTransmitProbability = 
        []
    , pacCreateProbability = []
    , pacSwapProbability = []
    , pacUCreateProbability = []
    , pacCreateWerner = []
    , pacUCreateWerner = []
    , pacCoherenceTime = [("A", tCoh), ("B", tCoh)]
    , pacDistances = [(("A", "B"), 1)]
    }

main :: IO ()
main = 
    let ev = "A" ~~? "B"
        tCoh = 100
    in qbkatMainD (actionConfig tCoh) nbUnbounded ev p initState