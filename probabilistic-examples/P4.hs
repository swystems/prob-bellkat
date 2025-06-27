{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import BellKAT.Implementations.ProbAtomicOneStepQuantum
import BellKAT.ProbabilisticPrelude

e :: ProbBellKATPolicy
e = ite ("C" /~? "C") (create "C") (trans "C" ("A", "C")) <> trans "C" ("A", "C")

f :: ProbBellKATPolicy
f = ite ("C" /~? "C") (create "C") (trans "C" ("A", "C")) <> trans "C" ("B", "C")

p :: ProbBellKATPolicy
p = e <||> f

actionConfig :: ProbabilisticActionConfiguration
actionConfig = PAC 
    { pacTransmitProbability = [(("C", "B"), 1 / 2),(("C", "A"), 4 / 5)] 
    , pacCreateProbability = [("C", 2 / 3)]
    , pacUCreateProbability = []
    , pacSwapProbability = [] 
    }

networkCapacity :: NetworkCapacity BellKATTag
networkCapacity = ["C" ~ "C", "C" ~ "C", "A" ~ "C", "B" ~ "C"]

main :: IO ()
main = do
    let ev = hasSubset ["A" ~ "C", "B" ~ "C"]
     in pbkatMain actionConfig (Just networkCapacity) ev p
