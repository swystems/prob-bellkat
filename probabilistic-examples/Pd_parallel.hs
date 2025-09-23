import BellKAT.ProbabilisticPrelude

p :: ProbBellKATPolicy
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
     in pbkatMain actionConfig Nothing ev p