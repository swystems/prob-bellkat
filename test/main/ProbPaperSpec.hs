{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module ProbPaperSpec where

import Test.Hspec
import qualified Numeric.Probability.Distribution as P

import BellKAT.Prelude
import BellKAT.Definitions.Policy (TaggedAction) 
import BellKAT.Definitions.Core (CreateBellPairArgs(..)) 
import BellKAT.Definitions.Atomic (createProbabilitsticAtomicAction)
import BellKAT.ActionEmbeddings
import BellKAT.PolicyEmbeddings 
import BellKAT.Utils.Automata.Guarded
import BellKAT.Utils.Automata.Transitions.Guarded
import BellKAT.Implementations.GuardedAutomataStepQuantum
import BellKAT.Implementations.ProbAtomicOneStepQuantum

type BellKATAutomaton = GuardedFA ProbBellKATTest (ProbAtomicOneStepPolicy BellKATTag)

-- | = Example 4.2

e42 :: ProbBellKATPolicy
e42 = ite ("C" /~? "C") (create "C") (trans "C" ("A", "C")) <> trans "C" ("A", "C")

e42FA :: BellKATAutomaton
e42FA = GFA 0 $ gtsFromList 
    [(0, [("C" /~? "C", Step (fromBasicAction $ create "C") 1)
         ,("C" ~~? "C", Step (fromBasicAction $ trans "C" ("A", "C")) 2)])
    ,(1, [(true, Step (fromBasicAction $ trans "C" ("A", "C")) 3)])
    ,(2, [(true, Step (fromBasicAction $ trans "C" ("A", "C")) 3)])
    ,(3, [(true, Done)])
    ]

e42FAp :: BellKATAutomaton
e42FAp = GFA 0 $ gtsFromList 
    [(0, [("C" /~? "C", Step 
            [createProbabilitsticAtomicAction [] [] (P.choose (2/3) ["C" ~ "C"] [])
            ,createProbabilitsticAtomicAction [[]] [] (P.certainly [])] 1)
         ,("C" ~~? "C", Step 
            [createProbabilitsticAtomicAction [] ["C" ~ "C"] (P.choose (4/5) ["A" ~ "C"] [])
            ,createProbabilitsticAtomicAction [["C" ~ "C"]] [] (P.certainly [])
            ] 2)])
    ,(1, [(true, Step 
            [createProbabilitsticAtomicAction [] ["C" ~ "C"] (P.choose (4/5) ["A" ~ "C"] [])
            ,createProbabilitsticAtomicAction [["C" ~ "C"]] [] (P.certainly [])
            ] 3)])
    ,(2, [(true, Step 
            [createProbabilitsticAtomicAction [] ["C" ~ "C"] (P.choose (4/5) ["A" ~ "C"] [])
            ,createProbabilitsticAtomicAction [["C" ~ "C"]] [] (P.certainly [])
            ] 3)])
    ,(3, [(true, Done)])
    ]

f42 :: ProbBellKATPolicy
f42 = create "C" <> ite ("C" /~? "C") (create "C") (trans "C" ("B", "C"))

f42FA :: BellKATAutomaton
f42FA = GFA 0 $ gtsFromList 
    [(0, [(true, Step (fromBasicAction $ create "C") 1)])
    ,(1, [("C" /~? "C", Step (fromBasicAction $ create "C") 2)
         ,("C" ~~? "C", Step (fromBasicAction $ trans "C" ("B", "C")) 3)])
    ,(2, [(true, Done)])
    ,(3, [(true, Done)])
    ]

ef42 :: ProbBellKATPolicy
ef42 = e42 <||> f42

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
        it "correctly represents example 4.2 det (e)" $
            asAutomaton e42 `shouldBe` e42FA
        it "correctly represents example 4.2 det (f)" $
            asAutomaton f42 `shouldBe` f42FA
        it "correctly represents example 4.2 prob (e)" $
            asAutomatonP e42 `shouldBe` e42FAp
        it "correctly represents example 4.2 prob (e || f)" $
            asAutomatonP ef42 `shouldBe` e42FAp

fromBasicAction :: CreatesBellPairs a BellKATTag => TaggedAction BellKATTag -> a
fromBasicAction = tryCreateBellPairFrom . simpleActionMeaning


pac :: ProbabilisticActionConfiguration
pac = PAC [(("C", "A"), 4 / 5)] [("C", 2 / 3)]

probActionMeaning :: TaggedAction t -> CreateBellPairArgs t
probActionMeaning = probabilisticActionMeaning pac

fromBasicActionP :: CreatesBellPairs a BellKATTag => TaggedAction BellKATTag -> a
fromBasicActionP = tryCreateBellPairFrom . probActionMeaning

asAutomaton :: ProbBellKATPolicy -> BellKATAutomaton 
asAutomaton x = getGFA (meaning . mapDesugarActions simpleActionMeaning $ x)

asAutomatonP :: ProbBellKATPolicy -> BellKATAutomaton 
asAutomatonP x = getGFA (meaning . mapDesugarActions probActionMeaning $ x)
