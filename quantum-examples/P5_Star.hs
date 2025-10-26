import BellKAT.QuantumPrelude

pG :: Int -> QBKATPolicy
pG nG =
    whileN nG ("A" /~? "H") (ucreate ("A", "H"))
        <||>
    whileN nG ("B" /~? "H") (ucreate ("B", "H"))
        <||>
    whileN nG ("C" /~? "H") (ucreate ("C", "H"))

p :: Int -> Int -> Location -> QBKATPolicy
p nS nG priority =
    if priority == "A"
        then
            whileN nS ("A" /~? "C" ||* "B" /~? "C") (pG nG <> (swap "H" ("A", "C") <.> swap "H" ("B", "C")))
        else
            whileN nS ("A" /~? "C" ||* "B" /~? "C") (pG nG <> (swap "H" ("B", "C") <.> swap "H" ("A", "C")))


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
    let priority = "A"
        ev       = priority ~~? "C"
        p_gen    = 1/4
        p_swap   = 1/2
        w0       = 95/100
        tCoh     = 100
     in qbkatMainD (actionConfig p_gen w0 p_swap tCoh) (Just networkCapacity) ev (p 3 3 priority) mempty