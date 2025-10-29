import BellKAT.ProbabilisticPrelude

p :: Int -> ProbBellKATPolicy
p n =
    whileN n ("A" /~? "D")
    (   
        (
            ite ("B" /~? "D")
            (  
                (
                    (ite ("B" /~? "C") (ucreate ("B", "C")) mempty)
                        <||>
                    (ite ("C" /~? "D") (ucreate ("C", "D")) mempty)
                )
                <>
                (
                    swap "C" ("B", "D")
                )
            )
            mempty
            <||>
            ite ("A" /~? "B")
            (
                (ucreate ("A", "B"))
            )
            mempty
        )
        <> (swap "B" ("A", "D"))
    )

actionConfig :: Rational -> Rational -> ProbabilisticActionConfiguration
actionConfig  p_gen p_swap =
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
        p_gen  = 1/4
        p_swap = 3/4
     in pbkatMainD (actionConfig p_gen p_swap) Nothing ev (p 54)