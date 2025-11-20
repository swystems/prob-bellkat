import BellKAT.QuantumPrelude

pG :: QBKATPolicy
pG =
    (
        ite ("A" /~? "B") (ucreate ("A", "B")) mempty
        <||>
        ite ("B" /~? "C") (ucreate ("B", "C")) mempty
        <||>
        ite ("C" /~? "D") (ucreate ("C", "D")) mempty
    )
        <>
    (
        ite ("A" /~? "C") (swap "B" ("A", "C")) mempty
        <||>
        ite ("B" /~? "D") (swap "C" ("B", "D")) mempty
    )
        <>
    (
        swap "B" ("A", "D")
        <||>
        swap "C" ("A", "D")
    )

p :: Int -> QBKATPolicy
p n = whileN n ("A" /~? "D") pG

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "B", "B" ~ "C", "C" ~ "D", "A" ~ "C", "B" ~ "D", "A" ~ "D"]

nb :: NetworkBounds QBKATTag
nb = (NetworkBounds { nbCapacity = Just networkCapacity, nbCutoff = Nothing })

actionConfig :: Rational -> Double -> Rational -> ProbabilisticActionConfiguration
actionConfig p_gen w0 p_swap =
    PAC
        { pacTransmitProbability = []
        , pacCreateProbability = []
        , pacCreateWerner = []
        , pacUCreateProbability =
            [ (("A", "B"), p_gen)
            , (("B", "C"), p_gen)
            , (("C", "D"), p_gen)
            ]
        , pacSwapProbability =
            [ ("B", p_swap),
              ("C", p_swap)
            ]
        , pacUCreateWerner =
            [ (("A", "B"), w0)
            , (("B", "C"), w0)
            , (("C", "D"), w0)
            ]
        , pacCoherenceTime =
            [ ("A", 100000)
            , ("B", 100000)
            , ("C", 100)
            , ("D", 100)
            ]
        , pacDistances =
            [ (("A", "B"), 1)
            , (("B", "C"), 1)
            , (("C", "D"), 1)
            , (("A", "C"), 2)
            , (("B", "D"), 2)
            , (("A", "D"), 3)
            ]
        }

main :: IO ()
main =
    let ev     = "A" ~~? "D"
        p_gen  = 1/3
        p_swap = 1/2
        w0     = 958/1000
    in qbkatMainD (actionConfig p_gen w0 p_swap) nb ev (p 3) mempty