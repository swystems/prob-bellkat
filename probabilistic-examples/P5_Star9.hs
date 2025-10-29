import BellKAT.ProbabilisticPrelude

p :: Int -> ProbBellKATPolicy
p nS =
        whileN nS ("A" /~? "C" ||* "B" /~? "C")
        (   
            (   -- generations in parallel
                ite ("A" /~? "H") (ucreate ("A", "H")) mempty
                    <||>
                ite ("B" /~? "H") (ucreate ("B", "H")) mempty
                    <||>
                ite ("C" /~? "H") (ucreate ("C", "H")) mempty
            )
            <> -- followed by.. 
            (
                swap "H" ("A", "C") 
                <||>
                swap "H" ("B", "C")
            )
        ) 


networkCapacity :: NetworkCapacity BellKATTag
networkCapacity = ["A" ~ "H", "B" ~ "H", "C" ~ "H", "C" ~ "H", "A" ~ "C", "B" ~ "C"]

actionConfig :: Rational -> Rational -> ProbabilisticActionConfiguration
actionConfig p_gen p_swap =
    PAC
        { pacTransmitProbability = []
        , pacCreateProbability = []
        , pacUCreateProbability =
            [ (("A", "H"), p_gen)
            , (("B", "H"), p_gen)
            , (("C", "H"), p_gen)
            ]
        , pacSwapProbability =
            [ ("H", p_swap) ]
        }

main :: IO ()
main =
    let ev       = "A" ~~? "C"
        p_gen    = 1/4
        p_swap   = 1/2
     in pbkatMainD (actionConfig p_gen p_swap) (Just networkCapacity) ev (p 9)