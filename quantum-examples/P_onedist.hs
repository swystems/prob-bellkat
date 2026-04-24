import BellKAT.QuantumPrelude

p :: QBKATPolicy 
p = while (hasNotSubset ["A" ~ "B" .~ (1 :: QBKATTag)])
    (       
        -- if I already have a pair, generate only one more, 
        -- otherwise generate two pairs
        (
            ite ("A" /~? "B") (ucreate ("A", "B")) mempty
        <||> 
            ucreate ("A", "B")
        )
        <> 
            distill ("A", "B") .~ (1 :: QBKATTag)
    )

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "B", "A" ~ "B"]

nb :: NetworkBounds QBKATTag
nb = (NetworkBounds { nbCapacity = Just networkCapacity, nbCutoff = Nothing })

actionConfig :: Double -> Int -> ProbabilisticActionConfiguration
actionConfig w0 tCoh = PAC 
    { pacTransmitProbability = []
    , pacCreateProbability = []
    , pacCreateWerner = []
    , pacUCreateProbability = [(("A", "B"), 1/3)]
    , pacUCreateWerner = [(("A", "B"), w0)]
    , pacSwapProbability = []
    , pacCoherenceTime = [("A", tCoh), ("B", tCoh)]
    , pacDistances =
    [ (("A", "B"), 1)
    ]
    }

main :: IO ()
main = 
    let ev  = hasPureSubset ["A" ~ "B" .~ (1 :: QBKATTag)]
        w0  = 8/10
        tCoh = 10
    in qbkatMainD (actionConfig w0 tCoh) nb ev p mempty
