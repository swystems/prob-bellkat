{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module ProbPaperSpec where

import Test.Hspec

import BellKAT.Prelude 
import BellKAT.ActionEmbeddings
import BellKAT.PolicyEmbeddings 
import BellKAT.Utils.Automata.Guarded
import BellKAT.Utils.Automata.Transitions.Guarded
import BellKAT.Implementations.GuardedAutomataStepQuantum
import BellKAT.Implementations.ProbAtomicOneStepQuantum

-- | = Example 4.2

e42 :: ProbBellKATPolicy
e42 = ite ("C" /~? "C") (create "C") (trans "C" ("A", "C")) <> trans "C" ("A", "C")

e42FA :: GuardedFA ProbBellKATTest (ProbAtomicOneStepPolicy BellKATTag)
e42FA = GFA 0 $ gtsFromList 
    [(0, [("C" /~? "C", Step (tryCreateBellPairFrom . simpleActionMeaning $ create "C") 1)
         ,("C" ~~? "C", Step (tryCreateBellPairFrom . simpleActionMeaning $ trans "C" ("A", "C")) 2)])
    ,(1, [(true, Step (tryCreateBellPairFrom . simpleActionMeaning $ trans "C" ("A", "C")) 3)])
    ,(2, [(true, Step (tryCreateBellPairFrom . simpleActionMeaning $ trans "C" ("A", "C")) 3)])
    ,(3, [(true, Done)])
    ]

f42 :: ProbBellKATPolicy
f42 = create "C" <> ite ("C" /~? "C") (create "C") (trans "C" ("B", "C"))

-- | = Example 5.1

-- | == I

e51 :: ProbBellKATPolicy
e51 = create "C" <> trans "C" ("A", "C")

f51 :: ProbBellKATPolicy
f51 = create "C" <> trans "C" ("B", "C")

p51i :: ProbBellKATPolicy
p51i = e51 <||> f51

p51i' :: ProbBellKATPolicy
p51i' = e51 <.> f51

-- | == II

p51ii :: ProbBellKATPolicy
p51ii = p51i <> p51i

p51ii' :: ProbBellKATPolicy
p51ii' = p51i' <> p51i'

-- | == III

e51' :: ProbBellKATPolicy
e51' = create "C" <> ite ("A" /~? "C") (trans "C" ("A", "C")) (trans "C" ("B", "C"))

f51' :: ProbBellKATPolicy
f51' = create "C" <> ite ("A" /~? "C") (trans "C" ("B", "C")) (trans "C" ("A", "C")) 

p51iii :: ProbBellKATPolicy
p51iii = e51' <||> f51'

p51iii' :: ProbBellKATPolicy
p51iii' = p51iii <> p51iii

-- | == IV TODO: don't have star yet

-- | = Example 5.3 TODO: don't have star yet

spec :: Spec
spec = do
    describe "GuardedAutomatonStepQuantum" $ do
        it "correctly represents example 4.2 (e)" $
            getGFA (meaning . mapDesugarActions simpleActionMeaning $ e42) `shouldBe` e42FA

