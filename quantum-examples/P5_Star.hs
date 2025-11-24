import BellKAT.QuantumPrelude

p :: Int -> QBKATPolicy
p nS =
        whileN nS ("A" /~? "C" &&* "B" /~? "C")
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

nb :: NetworkBounds QBKATTag
nb = (NetworkBounds { nbCapacity = Just networkCapacity, nbCutoff = Nothing })

actionConfig :: ProbabilisticActionConfiguration
actionConfig =
    PAC
        { pacTransmitProbability = []
        , pacCreateProbability = []
        , pacCreateWerner = []
        , pacUCreateProbability =
            [ (("A", "H"), 1/4)
            , (("B", "H"), 1/3)
            , (("C", "H"), 1/3)
            ]
        , pacSwapProbability =
            [ ("H", 1/2) ]
        , pacUCreateWerner =
            [ (("A", "H"), 90/100)
            , (("B", "H"), 95/100)
            , (("C", "H"), 95/100)
            ]
        , pacCoherenceTime =
            [ ("A", 100)
            , ("B", 100)
            , ("C", 100)
            , ("H", 200)
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
    in qbkatMainD actionConfig nb ev (p 54) mempty