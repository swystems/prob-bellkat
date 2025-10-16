import BellKAT.QuantumPrelude

pG :: QBKATPolicy
pG =
    ite ("A" /~? "B") (ucreate ("A", "B")) mempty
        <||>
    ite ("B" /~? "C") (ucreate ("B", "C")) mempty
        <||>
    ite ("C" /~? "D") (ucreate ("C", "D")) mempty
        <||>
    ite ("A" /~? "C") (swap "B" ("A", "C")) mempty
        <||>
    ite ("B" /~? "D") (swap "C" ("B", "D")) mempty
        <||>
    swap "B" ("A", "D")
        <||>
    swap "C" ("A", "D")

p :: Int -> QBKATPolicy
p n = whileN n ("A" /~? "D") pG

networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "B", "B" ~ "C", "C" ~ "D", "A" ~ "C", "B" ~ "D", "A" ~ "D"]

actionConfig :: Rational -> Rational -> ProbabilisticActionConfiguration
actionConfig p_gen p_swap =
    PAC
        { pacTransmitProbability = []
        , pacCreateProbability = []
        , pacUCreateProbability =
            [ (("A", "B"), p_gen)
            , (("B", "C"), p_gen)
            , (("C", "D"), p_gen)
            ]
        , pacSwapProbability =
            [ ("B", p_swap),
              ("C", p_swap)
            ]
        }

main :: IO ()
main =
    let ev     = "A" ~~? "D"
        p_gen  = 8/10
        p_swap = 9/10
     in qbkatMainD (actionConfig p_gen p_swap) (Just networkCapacity) ev (p 2) mempty