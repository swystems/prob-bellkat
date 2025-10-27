import BellKAT.QuantumPrelude

p :: Int -> QBKATPolicy
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
                <.>
                swap "H" ("B", "C")
            )
        ) 


networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "H", "B" ~ "H", "C" ~ "H", "C" ~ "H"]

actionConfig :: Rational -> Double -> Rational -> Int -> ProbabilisticActionConfiguration
actionConfig p_gen w0 p_swap tCoh =
    PAC
        { pacTransmitProbability = []
        , pacCreateProbability = []
        , pacCreateWerner = []
        , pacUCreateProbability =
            [ (("A", "H"), p_gen)
            , (("B", "H"), p_gen)
            , (("C", "H"), p_gen)
            ]
        , pacSwapProbability =
            [ ("H", p_swap) ]
        , pacUCreateWerner =
            [ (("A", "H"), w0)
            , (("B", "H"), w0)
            , (("C", "H"), w0)
            ]
        , pacCoherenceTime =
            [ ("A", tCoh)
            , ("B", tCoh)
            , ("C", tCoh)
            , ("H", tCoh)
            ]
        , pacDistances =
            [ (("A", "H"), 1)
            , (("B", "H"), 1)
            , (("C", "H"), 1)
            ]
        }

main :: IO ()
main =
    let ev       = "A" ~~? "C"
        p_gen    = 1/4
        p_swap   = 1/2
        w0       = 95/100
        tCoh     = 100
     in qbkatMainD (actionConfig p_gen w0 p_swap tCoh) (Just networkCapacity) ev (p 27) mempty