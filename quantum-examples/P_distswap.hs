import BellKAT.QuantumPrelude

p :: QBKATPolicy 
p = while (hasNotSubset ["A" ~ "C" .~ (1 :: QBKATTag)])
    (       
        -- gen
        -- if I already have a distilled pair, do not generate more
        (
            ite (hasNotSubset ["A" ~ "B" .~ (1 :: QBKATTag)])
            (
                -- if I already have a pair, generate only one more,
                -- otherwise generate two pairs
                ite ("A" /~? "B") (ucreate ("A", "B")) mempty
            <||>
                ucreate ("A", "B")
            )
            mempty
        <||>
            ite (hasNotSubset ["B" ~ "C" .~ (1 :: QBKATTag)])
            (
                ite ("B" /~? "C") (ucreate ("B", "C")) mempty
            <||>
                ucreate ("B", "C")
            )
            mempty
        )
        -- distill
        -- if I already have a distilled pair, do not distill
        <> 
        (
            ite (hasNotSubset ["A" ~ "B" .~ (1 :: QBKATTag)]) (distill ("A", "B") .~ (1 :: QBKATTag)) mempty
        <||>
            ite (hasNotSubset ["B" ~ "C" .~ (1 :: QBKATTag)]) (distill ("B", "C") .~ (1 :: QBKATTag)) mempty
        )
        -- swap
        <>
            (1 :: QBKATTag) ~. swap "B" ("A", "C")
    )

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "B", "A" ~ "B", "B" ~ "C", "B" ~ "C", "A" ~ "C"]

nb :: NetworkBounds QBKATTag
nb = (NetworkBounds { nbCapacity = Just networkCapacity, nbCutoff = Nothing })

actionConfig :: Double -> Int -> ProbabilisticActionConfiguration
actionConfig w0 tCoh = PAC 
    { pacTransmitProbability = []
    , pacCreateProbability = []
    , pacCreateWerner = []
    , pacUCreateProbability = [(("A", "B"), 1/3), (("B", "C"), 1/5)]
    , pacUCreateWerner = [(("A", "B"), w0), (("B", "C"), w0)]
    , pacSwapProbability = [("B", 1/2)]
    , pacCoherenceTime = [("A", tCoh), ("B", tCoh), ("C", tCoh)]
    , pacDistances =
    [ (("A", "B"), 1)
    , (("B", "C"), 1)
    , (("A", "C"), 2)
    ]
    }

main :: IO ()
main = 
    let ev  = hasPureSubset ["A" ~ "C" .~ (1 :: QBKATTag)]
        w0  = 8/10
        tCoh = 10
    in qbkatMainD (actionConfig w0 tCoh) nb ev p mempty
