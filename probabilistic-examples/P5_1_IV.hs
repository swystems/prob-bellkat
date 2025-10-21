import BellKAT.ProbabilisticPrelude

e :: ProbBellKATPolicy
e = create "C" <> ite ("A" /~? "C") (trans "C" ("A", "C")) (trans "C" ("B", "C"))

f :: ProbBellKATPolicy
f = create "C" <> ite ("B" /~? "C") (trans "C" ("B", "C")) (trans "C" ("A", "C"))

p :: ProbBellKATPolicy
p = whileN 3 ("A" /~? "C" ||* "B" /~? "C") (e <||> f)

networkCapacity :: NetworkCapacity BellKATTag
networkCapacity = ["C" ~ "C", "C" ~ "C", "A" ~ "C", "B" ~ "C"]

actionConfig :: ProbabilisticActionConfiguration
actionConfig = PAC 
    { pacTransmitProbability = [(("C", "B"), 1 / 2),(("C", "A"), 4 / 5)]
    , pacCreateProbability = [("C", 9/10)]
    , pacCreateWerner = [("C", 1.0)]
    , pacUCreateProbability = []
    , pacUCreateWerner = []
    , pacSwapProbability = []
    , pacCoherenceTime = [("A", 1), ("B", 1), ("C", 1)]
    }

main :: IO ()
main =
    let ev = hasSubset ["A" ~ "C", "B" ~ "C"]
     in pbkatMain actionConfig (Just networkCapacity) ev p

