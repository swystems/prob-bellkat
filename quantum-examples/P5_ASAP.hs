import BellKAT.QuantumPrelude

pG :: QBKATPolicy
pG =
    (ite ("A" /~? "B") (ucreate ("A", "B")) mempty
        <||>
    ite ("B" /~? "C") (ucreate ("B", "C")) mempty
        <||>
    ite ("C" /~? "D") (ucreate ("C", "D")) mempty)
        <>
    (ite ("A" /~? "C") (swap "B" ("A", "C")) mempty
        <.>
    ite ("B" /~? "D") (swap "C" ("B", "D")) mempty)
        <>
    (swap "B" ("A", "D")
        <.>
    swap "C" ("A", "D"))

p :: Int -> QBKATPolicy
p n = whileN n ("A" /~? "D") pG

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "B", "B" ~ "C", "C" ~ "D", "A" ~ "C", "B" ~ "D", "A" ~ "D"]

actionConfig :: Rational -> Double -> Rational -> Int -> ProbabilisticActionConfiguration
actionConfig p_gen w0 p_swap tCoh =
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
            [ ("A", tCoh)
            , ("B", tCoh)
            , ("C", tCoh)
            , ("D", tCoh)
            ]
        }

main :: IO ()
main =
    let ev     = "A" ~~? "D"
        p_gen  = 8/10
        p_swap = 9/10
        w0     = 958/1000
        tCoh   = 100
     in qbkatMainD (actionConfig p_gen w0 p_swap tCoh) (Just networkCapacity) ev (p 7) mempty