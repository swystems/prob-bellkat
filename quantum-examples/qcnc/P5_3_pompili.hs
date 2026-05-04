import BellKAT.QuantumPrelude

oneAttempt :: QBKATPolicy
oneAttempt = 
    let n = 450 :: Int
     in (
         stimes n (ite ("A" /~? "C") (ucreate ("A", "C")) mempty) 
            <||> 
         stimes n (ite ("B" /~? "C") (ucreate ("B", "C")) mempty)
        ) 
        <> swap "C" ("A", "B")

p :: Int -> QBKATPolicy 
p n = whileN n ("A" /~? "C" ||* "B" /~? "C") oneAttempt

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "B", "A" ~ "C", "B" ~ "C"]

nb :: NetworkBounds QBKATTag
nb = def { nbCapacity = Just networkCapacity }

actionConfig :: Double -> Int -> ProbabilisticActionConfiguration
actionConfig w0 tCoh = PAC 
    { pacTransmitProbability = []
    , pacCreateProbability = []
    , pacCreateWerner = []
    , pacUCreateProbability = [(("A", "C"), 36/10000), (("B", "C"), 28/10000)]
    , pacUCreateWerner = [(("A", "C"), w0), (("B", "C"), w0)]
    , pacSwapProbability = [("C", 71/10000)]
    , pacCoherenceTime = [("A", tCoh), ("B", tCoh), ("C", tCoh)]
    , pacDistances =
    [ (("A", "B"), 1)
    , (("B", "C"), 1)
    , (("A", "C"), 2)
    ]
    }

main :: IO ()
main = 
    let ev  = "A" ~~? "B"
        w0  = 958/1000
        tCoh = 100
    in qbkatMainD (actionConfig w0 tCoh) nb ev (p 1) mempty
