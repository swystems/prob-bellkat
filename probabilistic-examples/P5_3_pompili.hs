import BellKAT.ProbabilisticPrelude

oneAttempt :: ProbBellKATPolicy
oneAttempt = 
    let n = 450 :: Int
     in (stimes n (ite ("A" /~? "C") (ucreate ("A", "C")) mempty) 
            <||> stimes n (ite ("B" /~? "C") (ucreate ("B", "C")) mempty)) 
        <> swap "C" ("A", "B")

p :: Int -> ProbBellKATPolicy 
p n = whileN n ("A" /~? "C" ||* "B" /~? "C") oneAttempt

networkCapacity :: NetworkCapacity BellKATTag
networkCapacity = ["A" ~ "B", "A" ~ "C", "B" ~ "C"]

actionConfig :: ProbabilisticActionConfiguration
actionConfig = PAC 
    { pacTransmitProbability = []
    , pacCreateProbability = []
    , pacUCreateProbability = [(("A", "C"), 36/10000), (("B", "C"), 28/10000)]
    , pacSwapProbability = [("C", 71/10000)]
    }

main :: IO ()
main = 
    let result = applyProbStarPolicyD actionConfig (Just networkCapacity) (p 1) []
        ev = "A" ~~? "B"
     in pbkatMain result ev

