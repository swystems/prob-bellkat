import BellKAT.ProbabilisticPrelude

p :: ProbBellKATPolicy
p = (create "C" <||> create "C") 
    <> 
    (trans "C" ("A", "C") <||> trans "C" ("B", "C"))
    <>
    (swap "C" ("A", "B"))

actionConfig :: ProbabilisticActionConfiguration
actionConfig = PAC 
    { pacTransmitProbability = 
        [(("C", "A"), 8/10)
        ,(("C", "B"), 7/10)
        ]
    , pacCreateProbability = [("C", 9/10)]
    , pacCreateWerner = [("C", 1.0)]
    , pacSwapProbability = [("C", 6/10)]
    , pacUCreateProbability = []
    , pacUCreateWerner = []
    , pacCoherenceTime = [("A", 1), ("B", 1), ("C", 1)]
    }

main :: IO ()
main =
    let ev = "A" ~~? "B"
     in pbkatMain actionConfig Nothing ev p
