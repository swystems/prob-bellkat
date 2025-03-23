{-# OPTIONS_GHC -Wno-type-defaults #-}

import BellKAT.ProbabilisticPrelude

i :: BellKATTagChar
i = BellKATTagChar 'I'

o :: BellKATTagChar
o = BellKATTagChar 'O'

e :: Int -> Int -> ProbBellKATPolicy
e n k = 
    whileN n ("A" /~? "C") $
        (whileN k (hasNotSubset ["A" ~ "C", "A" ~ "C"])
            (create "C" .~ o <.> create "C" .~ o) <> (o ~. trans "C" ("A", "C") <.> o ~. trans "C" ("A", "C")))
        <> distill ("A", "C")

e' :: Int -> Int -> ProbBellKATPolicy
e' n k = 
    whileN n ("B" /~? "C") $
        (whileN k (hasNotSubset ["B" ~ "C", "B" ~ "C"])
            (create "C" .~ i <.> create "C" .~ i) <> (i ~. trans "C" ("B", "C") <.> i ~. trans "C" ("B", "C")))
        <> distill ("B", "C")

p :: Int -> Int -> ProbBellKATPolicy
p n k = (e n k <||> e' n k) <> swap "C" ("A", "B")

networkCapacity :: NetworkCapacity BellKATTag
networkCapacity = mconcat 
    [ stimes 2 ["C" ~ "C" .~ o]
    , stimes 2 ["C" ~ "C" .~ i]
    , stimes 2 ["A" ~ "C"]
    , stimes 2 ["B" ~ "C"]
    , ["A" ~ "B"]
    ]

actionConfig :: ProbabilisticActionConfiguration
actionConfig = PAC 
    { pacTransmitProbability = [(("C", "B"), 1 / 2),(("C", "A"), 4 / 5)]
    , pacCreateProbability = [("C", 9/10)]
    , pacUCreateProbability = [(("A", "C"), 36/10000), (("B", "C"), 28/10000)]
    , pacSwapProbability = [("C", 32/1000)]
    }

main :: IO ()
main = 
    let cdbps = applyProbStarPolicyD actionConfig (Just networkCapacity) (p 8 50) []
        ev = "A" ~~? "B"
     in do
        pbkatMain cdbps ev
