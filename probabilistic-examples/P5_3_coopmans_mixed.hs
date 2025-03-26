{-# OPTIONS_GHC -Wno-type-defaults #-}

import BellKAT.ProbabilisticPrelude

e :: Int -> Int -> ProbBellKATPolicy
e n k = 
    whileN n ("A" /~? "C") $
        (whileN k (hasNotSubset ["A" ~ "C", "A" ~ "C"])
            (ucreate ("A", "C") <.> ucreate ("A", "C")))
        <> (ite (hasSubset ["A" ~ "C", "A" ~ "C"]) (distill ("A", "C")) (destroy ("A","C")))

e' :: Int -> Int -> ProbBellKATPolicy
e' n k = 
    whileN n ("B" /~? "C") $
        (whileN k (hasNotSubset ["B" ~ "C", "B" ~ "C"])
            (ucreate ("B", "C") <.> ucreate ("B", "C")))
        <> (ite (hasSubset ["B" ~ "C", "B" ~ "C"]) (distill ("B", "C")) (destroy ("B","C")))

p :: Int -> Int -> ProbBellKATPolicy
p n k = (e n k <||> e' n k) <> swap "C" ("A", "B")

networkCapacity :: NetworkCapacity BellKATTag
networkCapacity = mconcat 
    [ stimes 2 ["C" ~ "C"]
    , stimes 2 ["C" ~ "C"]
    , stimes 2 ["A" ~ "C"]
    , stimes 2 ["B" ~ "C"]
    , ["A" ~ "B"]
    ]

actionConfig :: ProbabilisticActionConfiguration
actionConfig = PAC 
    { pacTransmitProbability = []
    , pacCreateProbability = []
    , pacUCreateProbability = [(("A", "C"), 36/10000), (("B", "C"), 28/10000)]
    , pacSwapProbability = [("C", 71/10000)]
    }

main :: IO ()
main = 
    let cdbps = applyProbStarPolicyD actionConfig (Just networkCapacity) (p 9 49) []
        ev = "A" ~~? "B"
     in do
        pbkatMain cdbps ev
