import BellKAT.ProbabilisticPrelude

e :: ProbBellKATPolicy
e = create "C" <> ite ("A" /~? "C") (trans "C" ("A", "C")) (trans "C" ("B", "C"))

f :: ProbBellKATPolicy
f = create "C" <> ite ("B" /~? "C") (trans "C" ("B", "C")) (trans "C" ("A", "C"))

p :: ProbBellKATPolicy
p = (e <||> f) <> (e <||> f)

networkCapacity :: NetworkCapacity BellKATTag
networkCapacity = ["C" ~ "C", "C" ~ "C", "A" ~ "C", "B" ~ "C"]

actionConfig :: ProbabilisticActionConfiguration
actionConfig = PAC 
    { pacTransmitProbability = [(("C", "B"), 1 / 2),(("C", "A"), 4 / 5)]
    , pacCreateProbability = [("C", 9/10)]
    , pacUCreateProbability = []
    , pacSwapProbability = []
    }

main :: IO ()
main =
    let cdbps = applyProbStarPolicy actionConfig (Just networkCapacity) p []
        ev = hasSubset ["A" ~ "C", "B" ~ "C"]
     in pbkatMain cdbps ev 

