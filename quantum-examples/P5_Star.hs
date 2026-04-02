import BellKAT.QuantumPrelude

p :: QBKATPolicy
p =
        while ("A" /~? "C" &&* "B" /~? "C")
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


networkCapacity :: NetworkCapacity QBKATTag
networkCapacity = ["A" ~ "H", "B" ~ "H", "C" ~ "H", "A" ~ "C", "B" ~ "C"]

nb :: NetworkBounds QBKATTag
nb = (NetworkBounds { nbCapacity = Just networkCapacity, nbCutoff = Nothing })

actionConfig :: ProbabilisticActionConfiguration
actionConfig =
    PAC
        { pacTransmitProbability = []
        , pacCreateProbability = []
        , pacCreateWerner = []
        , pacUCreateProbability =
            [ (("A", "H"), 1/40000)
            , (("B", "H"), 1/30000)
            , (("C", "H"), 1/30000)
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
    let ev = "B" ~~? "C"
    in qbkatMainD actionConfig nb ev p mempty