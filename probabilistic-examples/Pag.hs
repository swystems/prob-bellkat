import BellKAT.ProbabilisticPrelude

e :: ProbBellKATPolicy
e = (ite ("C" /~? "C") (create "C") (trans "C" ("A", "C"))) <> trans "C" ("A", "C")

f :: ProbBellKATPolicy
f = (create "C") <> (ite ("C" /~? "C") (create "C") (trans "C" ("B", "C")))

p :: ProbBellKATPolicy
p = (e <||> f) <> swap "C" ("A", "B")

actionConfig :: ProbabilisticActionConfiguration
actionConfig = PAC 
    { pacTransmitProbability = 
        [(("C", "A"), 4/5)
        ,(("C", "B"), 1/2)
        ]
    , pacCreateProbability = [("C", 2/3)]
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
