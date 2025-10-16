import BellKAT.QuantumPrelude

pG1 :: Int -> QBKATPolicy
pG1 n =
    whileN n ("A" /~? "B") (ucreate ("A", "B"))
        <||>
    whileN n ("B" /~? "C") (ucreate ("B", "C"))

pG2 :: Int -> QBKATPolicy
pG2 n =
    whileN n ("C" /~? "D") (ucreate ("C", "D"))
        <||>
    whileN n ("D" /~? "E") (ucreate ("D", "E"))

pS :: Int -> QBKATPolicy
pS n =
    whileN n ("A" /~? "C") (pG1 n <> swap "B" ("A", "C"))
        <||>
    whileN n ("C" /~? "E") (pG2 n <> swap "D" ("C", "E"))

p :: Int -> QBKATPolicy
p n = whileN n ("A" /~? "E") (pS n <> swap "C" ("A", "E"))

actionConfig :: Rational -> Rational -> ProbabilisticActionConfiguration
actionConfig p_gen p_swap =
    PAC
        { pacTransmitProbability = []
        , pacCreateProbability = []
        , pacUCreateProbability =
            [ (("A", "B"), p_gen)
            , (("B", "C"), p_gen)
            , (("C", "D"), p_gen)
            , (("D", "E"), p_gen)
            ]
        , pacSwapProbability =
            [ ("B", p_swap)
            , ("C", p_swap)
            , ("D", p_swap)
            ]
        }

main :: IO ()
main =
    let ev     = "A" ~~? "E"
        p_gen  = 26/10000
        p_swap = 75/100
     in qbkatMainD (actionConfig p_gen p_swap) Nothing ev (p 2) mempty