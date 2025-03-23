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
    , pacSwapProbability = [("C", 6/10)]
    , pacUCreateProbability = []
    }

main :: IO ()
main = 
    let cdbps = applyProbStarPolicy actionConfig Nothing p []
        ev = "A" ~~? "B"
     in pbkatMain cdbps ev
