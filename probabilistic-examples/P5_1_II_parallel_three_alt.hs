import BellKAT.ProbabilisticPrelude

e :: ProbBellKATPolicy
e = create "C" <> trans "C" ("A", "C")

f :: ProbBellKATPolicy
f = create "C" <> trans "C" ("B", "C")

p :: ProbBellKATPolicy
p = ((f <||> e) <> (f <||> e)) <> (f <||> e)

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

