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
p = distill ("A", "B") <||> distill ("A", "B")

actionConfig :: ProbabilisticActionConfiguration
actionConfig = PAC 
    { pacTransmitProbability = 
        []
    , pacCreateProbability = []
    , pacSwapProbability = []
    , pacUCreateProbability = []
    }


main :: IO ()
main = 
    let ev = "A" ~~? "B"
    in qbkatMainD actionConfig Nothing ev p initState