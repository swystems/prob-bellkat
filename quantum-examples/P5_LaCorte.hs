import BellKAT.QuantumPrelude

p :: Int -> QBKATPolicy
p n =
    whileN n ("A" /~? "D")
    (
        (
            ite ("B" /~? "D")
            (
                (
                    -- generations in parallel
                    ite ("B" /~? "C") (ucreate ("B", "C")) mempty
                        <||>
                    ite ("C" /~? "D") (ucreate ("C", "D")) mempty
                )
                <>
                swap "C" ("B", "D")
            )
            mempty
            <||>
            ite ("A" /~? "B")
            (
                ucreate ("A", "B")
            )
            mempty
        )
        <> swap "B" ("A", "D")
    )

actionConfig :: Bool -> Rational -> Double -> Rational -> Int -> ProbabilisticActionConfiguration
actionConfig useFirst pGen w0 pSwap tCoh =
    PAC
        { pacTransmitProbability = []
        , pacCreateProbability = []
        , pacCreateWerner = []
        , pacUCreateProbability =
            [ (("A", "B"), pGen)
            , (("B", "C"), pGen)
            , (("C", "D"), pGen)
            ]
                , pacSwapProbability =
            [ ("B", pSwap),
              ("C", pSwap)
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
        pGen  = 1/4
        pSwap = 3/4
        w0     = 95/100
        tCoh   = 1000
    in qbkatMainD (actionConfig useFirstExp pGen w0 pSwap tCoh) nbUnbounded ev (p 27) mempty