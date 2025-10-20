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
            whileN nS ("A" /~? "C") (pG nG <> swap "H" ("A", "C"))
                <.>
            whileN nS ("B" /~? "C") (pG nG <> swap "H" ("B", "C"))
        else
            whileN nS ("B" /~? "C") (pG nG <> swap "H" ("B", "C"))
                <.>
            whileN nS ("A" /~? "C") (pG nG <> swap "H" ("A", "C"))


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
        }

main :: IO ()
main =
    let priority = "A"
        ev       = priority ~~? "C"
        p_gen    = 1/5 {-1/10-}
        p_swap   = 3/4 {-1/2-}
        w0       = 958/1000
        tCoh     = 100
     in qbkatMainD (actionConfig p_gen w0 p_swap tCoh) (Just networkCapacity) ev (p 2 7 priority {-3 22-}) mempty