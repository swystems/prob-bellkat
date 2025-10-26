import BellKAT.QuantumPrelude

pG1 :: Int -> QBKATPolicy
pG1 n =
    whileN n ("A" /~? "B") (ucreate ("A", "B"))

pG2 :: Int -> QBKATPolicy
pG2 n =
    whileN n ("B" /~? "C") (ucreate ("B", "C"))
        <||>
    whileN n ("C" /~? "D") (ucreate ("C", "D"))

pS :: Int -> QBKATPolicy
pS n =
    whileN n ("B" /~? "D") ((pG1 n <||> pG2 n) <> swap "C" ("B", "D"))

p :: Int -> QBKATPolicy
p n = whileN n ("A" /~? "D") (pS n <> swap "B" ("A", "D"))

actionConfig :: Bool -> Rational -> Double -> Rational -> Int -> ProbabilisticActionConfiguration
actionConfig useFirst p_gen w0 p_swap tCoh =
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
                , pacUCreateWerner = if useFirst
                        then
                            -- 1st: heterogeneous Werner
                            [ (("A", "B"), w0)
                            , (("B", "C"), w0)
                            , (("C", "D"), 9/10)
                            ]
                        else
                            -- 2nd: homogeneous Werner
                            [ (("A", "B"), w0)
                            , (("B", "C"), w0)
                            , (("C", "D"), w0)
                            ]
                , pacCoherenceTime = if useFirst
                        then
                            -- 1st: homogeneous coherence times
                            [ ("A", tCoh)
                            , ("B", tCoh)
                            , ("C", tCoh)
                            , ("D", tCoh)
                            ]
                        else
                            -- 2nd: heterogeneous coherence times
                            [ ("A", tCoh)
                            , ("B", tCoh)
                            , ("C", tCoh)
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
        useFirstExp = True
        p_gen  = 1/4
        p_swap = 3/4
        w0     = 958/1000
        tCoh   = 1000
     in qbkatMainD (actionConfig useFirstExp p_gen w0 p_swap tCoh) Nothing ev (p 3) mempty