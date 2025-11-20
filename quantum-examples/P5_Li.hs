import BellKAT.QuantumPrelude

p :: Int -> QBKATPolicy
p n =
    whileN n ("A" /~? "C")
    (
        -- (
        --     -- AB: try to obtain two copies, then distill; otherwise destroy leftovers
        --     (
        --         ite (hasNotSubset ["A" ~ "B", "A" ~ "B"]) (ucreate ("A", "B") <.> ucreate ("A", "B")) mempty
        --         <>
        --         ite (hasSubset ["A" ~ "B", "A" ~ "B"]) (distill ("A", "B")) (destroy ("A", "B"))
        --     )
        --     <||>
        --     -- BC: try to obtain two copies, then distill; otherwise destroy leftovers
        --     (
        --         ite (hasNotSubset ["B" ~ "C", "B" ~ "C"]) (ucreate ("B", "C") <.> ucreate ("B", "C")) mempty
        --         <>
        --         ite (hasSubset ["B" ~ "C", "B" ~ "C"]) (distill ("B", "C")) (destroy ("B", "C"))
        --     )
        -- )
        (
            -- generations in parallel
            ite ("A" /~? "B") (ucreate ("A", "B")) mempty
            <||>
            ite ("B" /~? "C") (ucreate ("B", "C")) mempty
        )
        <>
        -- swap to create A~C
        swap "B" ("A", "C")
    )

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "B", "A" ~ "C", "B" ~ "C"]

cutoff :: CutoffSpec
cutoff = 8

nb :: NetworkBounds QBKATTag
nb = (NetworkBounds { nbCapacity = Just networkCapacity, nbCutoff = Just cutoff })

actionConfig :: Rational -> Double -> Rational -> Int -> ProbabilisticActionConfiguration
actionConfig pGen w0 pSwap tCoh =
    PAC
        { pacTransmitProbability = []
        , pacCreateProbability = []
        , pacCreateWerner = []
        , pacUCreateProbability =
            [ (("A", "B"), pGen)
            , (("B", "C"), pGen)
            ]
        , pacSwapProbability =
            [ ("B", pSwap)
            ]
        , pacUCreateWerner =
            [ (("A", "B"), w0)
            , (("B", "C"), w0)
            ]
        , pacCoherenceTime =
            [ ("A", tCoh)
            , ("B", tCoh)
            , ("C", tCoh)
        ]
        , pacDistances =
            [ (("A", "B"), 1)
            , (("B", "C"), 1)
            , (("A", "C"), 2)
            ]
        }

main :: IO ()
main =
    let ev     = "A" ~~? "C"
        pGen  = 1/4
        pSwap = 3/4
        w0     = 95/100
        tCoh   = 1000
    in qbkatMainD (actionConfig pGen w0 pSwap tCoh) nb ev (p 108) mempty