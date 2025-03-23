{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
import BellKAT.Utils.Convex as C

import BellKAT.Implementations.ProbAtomicOneStepQuantum
import BellKAT.ProbabilisticPrelude
import BellKAT.Definitions

e51' :: ProbBellKATPolicy
e51' = create "C" <> ite ("A" /~? "C") (trans "C" ("A", "C")) (trans "C" ("B", "C"))

f51' :: ProbBellKATPolicy
f51' = create "C" <> ite ("A" /~? "C") (trans "C" ("B", "C")) (trans "C" ("A", "C"))

p51iii :: ProbBellKATPolicy
p51iii = e51' <||> f51'

p51pac :: ProbabilisticActionConfiguration
p51pac = PAC [(("C", "B"), 1 / 2),(("C", "A"), 4 / 5)] [("C", 9/10)] [] []

p51iv :: Int -> ProbBellKATPolicy
p51iv n = whileN n ("A" /~? "C" ||* "B" /~? "C") p51iii

p51nc :: NetworkCapacity BellKATTag
p51nc = ["C" ~ "C", "C" ~ "C", "A" ~ "C", "B" ~ "C"]

main :: IO ()
main = do
        let result = applyProbStarPolicy' @_ @_ @Double p51pac (Just p51nc) (p51iv 4) []
        print result
        putStrLn $ "result size is " <> show (length $ C.getGenerators result)
