import BellKAT.QuantumPrelude

p :: Int -> QBKATPolicy
p n =
    whileN n ("A" /~? "C")
    (
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
cutoff = 4

nb :: NetworkBounds QBKATTag
nb = def
    { nbCapacity = Just networkCapacity
    , nbCutoff = Just cutoff
    , nbOperationTiming = InstantaneousOps
    }

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
        pSwap = 1/2
        w0     = 95/100
        tCoh   = 100
    in qbkatMainD (actionConfig pGen w0 pSwap tCoh) nb ev (p 108) mempty
