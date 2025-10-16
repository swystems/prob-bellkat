import BellKAT.QuantumPrelude

pG1 :: Int -> QBKATPolicy
pG1 itG =
    whileN itG ("A" /~? "H") (ucreate ("A", "H"))
        <||>
    whileN itG ("C" /~? "H") (ucreate ("C", "H"))

pG2 :: Int -> QBKATPolicy
pG2 itG =
    whileN itG ("B" /~? "H") (ucreate ("B", "H"))
        <||>
    whileN itG ("C" /~? "H") (ucreate ("C", "H"))

pS1 :: Int -> Int -> QBKATPolicy
pS1 itS itG =
    whileN itS ("A" /~? "C") oneAttempt
        where oneAttempt = pG1 itG <> swap "H" ("A", "C")

pS2 :: Int -> Int -> QBKATPolicy
pS2 itS itG =
    whileN itS ("B" /~? "C") oneAttempt
        where oneAttempt = pG2 itG <> swap "H" ("B", "C")

p :: Int -> Int -> QBKATPolicy
p itS itG = pS2 itS itG <.> pS1 itS itG

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
    let ev     = "B" ~~? "C"
        p_gen  = 1/5 {-1/100-}
        p_swap = 3/4
     in qbkatMainD (actionConfig p_gen p_swap) Nothing ev (p 2 1 {-4 460-}) mempty