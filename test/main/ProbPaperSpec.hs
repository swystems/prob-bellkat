{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module ProbPaperSpec where

import Control.Subcategory.Bind
import Control.Subcategory.Pointed
import Test.Hspec

import BellKAT.ProbabilisticPrelude
import BellKAT.Utils.Distribution as D
import BellKAT.Utils.Convex as C
import BellKAT.Definitions.Atomic (createProbabilitsticAtomicAction)
import BellKAT.ActionEmbeddings
import BellKAT.PolicyEmbeddings
import BellKAT.Utils.Automata.Guarded
import BellKAT.Utils.Automata.Transitions.Guarded
import BellKAT.Implementations.GuardedAutomataStepQuantum
import BellKAT.Implementations.ProbAtomicOneStepQuantum
import BellKAT.Definitions
type BellKATAutomaton = GuardedFA ProbBellKATTest (ProbAtomicOneStepPolicy BellKATTag)

-- | = Example 4.2

e42 :: ProbBellKATPolicy
e42 = ite ("C" /~? "C") (create "C") (trans "C" ("A", "C")) <> trans "C" ("A", "C")

f42 :: ProbBellKATPolicy
f42 = create "C" <> ite ("C" /~? "C") (create "C") (trans "C" ("B", "C"))

ef42 :: ProbBellKATPolicy
ef42 = e42 <||> f42

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
            [createProbabilitsticAtomicAction [] [] (D.choose (2/3) ["C" ~ "C"] [])
            ] 1)
         ,("C" ~~? "C", Step
            [createProbabilitsticAtomicAction [] ["C" ~ "C"] (D.choose (4/5) ["A" ~ "C"] [])
            ,createProbabilitsticAtomicAction [["C" ~ "C"]] [] (cpure [])
            ] 2)])
    ,(1, [(true, Step
            [createProbabilitsticAtomicAction [] ["C" ~ "C"] (D.choose (4/5) ["A" ~ "C"] [])
            ,createProbabilitsticAtomicAction [["C" ~ "C"]] [] (cpure [])
            ] 3)])
    ,(2, [(true, Step
            [createProbabilitsticAtomicAction [] ["C" ~ "C"] (D.choose (4/5) ["A" ~ "C"] [])
            ,createProbabilitsticAtomicAction [["C" ~ "C"]] [] (cpure [])
            ] 3)])
    ,(3, [(true, Done)])
    ]

f42FA :: BellKATAutomaton
f42FA = GFA 0 $ gtsFromList
    [(0, [(true, Step (fromBasicAction $ create "C") 1)])
    ,(1, [("C" /~? "C", Step (fromBasicAction $ create "C") 2)
         ,("C" ~~? "C", Step (fromBasicAction $ trans "C" ("B", "C")) 3)])
    ,(2, [(true, Done)])
    ,(3, [(true, Done)])
    ]

ef42FA :: BellKATAutomaton
ef42FA = GFA 0 $ gtsFromList
    [(0,
        [("C" /~? "C", Step 
            [createProbabilitsticAtomicAction [] [] (cpure ["C" ~ "C", "C" ~ "C"])] 1)
        ,("C" ~~? "C", Step 
            [createProbabilitsticAtomicAction [] ["C" ~ "C"] (cpure ["A" ~ "C", "C" ~ "C"])
            ,createProbabilitsticAtomicAction [["C" ~ "C"]] [] (cpure ["C" ~ "C"])
            ] 2)])
    ,(1,
        [("C" /~? "C", Step
            [createProbabilitsticAtomicAction [] ["C" ~ "C"] (cpure ["C" ~ "C", "A" ~ "C"])
            ,createProbabilitsticAtomicAction [["C" ~ "C"]] [] (cpure ["C" ~ "C"])
            ] 3)
        ,("C" ~~? "C", Step
            [createProbabilitsticAtomicAction [] ["C" ~ "C", "C" ~ "C"] (cpure ["A" ~ "C", "B" ~ "C"])
            ,createProbabilitsticAtomicAction [["C" ~ "C", "C" ~ "C"]] ["C" ~ "C"] (cpure ["A" ~ "C"])
            ,createProbabilitsticAtomicAction [["C" ~ "C", "C" ~ "C"]] ["C" ~ "C"] (cpure ["B" ~ "C"])
            ,createProbabilitsticAtomicAction [["C" ~ "C"]] [] (cpure [])
            ] 4)
        ])
    ,(2,
        [("C" /~? "C", Step
            [createProbabilitsticAtomicAction [] ["C" ~ "C"] (cpure ["C" ~ "C", "A" ~ "C"])
            ,createProbabilitsticAtomicAction [["C" ~ "C"]] [] (cpure ["C" ~ "C"])
            ] 3)
        ,("C" ~~? "C", Step
            [createProbabilitsticAtomicAction [] ["C" ~ "C", "C" ~ "C"] (cpure ["A" ~ "C", "B" ~ "C"])
            ,createProbabilitsticAtomicAction [["C" ~ "C", "C" ~ "C"]] ["C" ~ "C"] (cpure ["A" ~ "C"])
            ,createProbabilitsticAtomicAction [["C" ~ "C", "C" ~ "C"]] ["C" ~ "C"] (cpure ["B" ~ "C"])
            ,createProbabilitsticAtomicAction [["C" ~ "C"]] [] (cpure [])
            ] 4)
        ])
    ,(3, [(true, Done)])
    ,(4, [(true, Done)])
    ]

ef42FAp :: BellKATAutomaton
ef42FAp = GFA 0 $ gtsFromList
    [(0,
        [("C" /~? "C", Step [pCpC] 1)
        ,("C" ~~? "C", Step [pApC,pSpC] 2)])
    ,(1,
        [("C" /~? "C", Step [pApC, pSpC] 3)
        ,("C" ~~? "C", Step [pApB, pApS, pBpS, pSpS] 4)
        ])
    ,(2,
        [("C" /~? "C", Step [pApC, pSpC] 3)
        ,("C" ~~? "C", Step [pApB, pApS, pBpS, pSpS] 4)
        ])
    ,(3, [(true, Done)])
    ,(4, [(true, Done)])
    ]
  where
    pCpC = createProbabilitsticAtomicAction [] []
        [(["C" ~ "C", "C" ~ "C"], 4/9),(["C" ~ "C"], 4/9), ([], 1/9)]
    pApC = createProbabilitsticAtomicAction [] ["C" ~ "C"]
        [(["A" ~ "C", "C" ~ "C"], 8/15), (["C" ~ "C"], 2/15), (["A" ~ "C"], 4/15), ([], 1/15)]
    pSpC = createProbabilitsticAtomicAction [["C" ~ "C"]] []
        [(["C" ~ "C"], 2/3), ([], 1/3)]
    pApB = createProbabilitsticAtomicAction [] ["C" ~ "C", "C" ~ "C"]
        [(["A" ~ "C", "B" ~ "C"],4/10), (["A" ~ "C"], 4/10), (["B" ~ "C"], 1/10), ([],1/10) ]
    pApS = createProbabilitsticAtomicAction [["C" ~ "C", "C" ~ "C"]] ["C" ~ "C"]
        [(["A" ~ "C"], 4/5), ([], 1/5)]
    pSpS = createProbabilitsticAtomicAction [["C" ~ "C"]] [] $ cpure []
    pBpS = createProbabilitsticAtomicAction [["C" ~ "C", "C" ~ "C"]] ["C" ~ "C"]
        [(["B" ~ "C"], 1/2), ([], 1/2)]

-- | = Example 5.1

-- | == I

e51 :: ProbBellKATPolicy
e51 = create "C" <> trans "C" ("A", "C")

f51 :: ProbBellKATPolicy
f51 = create "C" <> trans "C" ("B", "C")

p51i :: ProbBellKATPolicy
p51i = e51 <||> f51

p51mu1 :: D' (TaggedBellPairs BellKATTag)
p51mu1 = [(["A" ~ "C", "B" ~ "C"], 324/1000), (["A" ~ "C"], 468/1000), (["B" ~ "C"], 81/1000), ([], 127/1000)]

p51mu2 :: D' (TaggedBellPairs BellKATTag)
p51mu2 = [(["A" ~ "C", "B" ~ "C"], 324/1000), (["A" ~ "C"], 324/1000), (["B" ~ "C"], 171/1000), ([], 181/1000)]

p51pac :: ProbabilisticActionConfiguration
p51pac = PAC [(("C", "B"), 1 / 2),(("C", "A"), 4 / 5)] [("C", 9/10)] [] []

p51nc :: NetworkCapacity BellKATTag
p51nc = ["C" ~ "C", "C" ~ "C", "A" ~ "C", "B" ~ "C"]

p51i' :: ProbBellKATPolicy
p51i' = e51 <.> f51

-- | == II

p51ii :: ProbBellKATPolicy
p51ii = p51i <> p51i

p51ii' :: ProbBellKATPolicy
p51ii' = p51i' <> p51i'

p51nu1 :: D' (TaggedBellPairs BellKATTag)
p51nu1 = [(["A" ~ "C", "B" ~ "C"], 61884/100000), (["A" ~ "C"], 337896/1000000), (["B" ~ "C"], 27135/1000000), ([], 16129/1000000)]

p51nu2 :: D' (TaggedBellPairs BellKATTag)
p51nu2 = [(["A" ~ "C", "B" ~ "C"], 653832/1000000), (["A" ~ "C"], 222264/1000000), (["B" ~ "C"], 91143/1000000), ([], 32761/1000000)]

p51nu3 :: D' (TaggedBellPairs BellKATTag)
p51nu3 = [(["A" ~ "C", "B" ~ "C"], 649296/1000000), (["A" ~ "C"], 277488/1000000), (["B" ~ "C"], 50229/1000000), ([], 22987/1000000)]

-- | == III

e51' :: ProbBellKATPolicy
e51' = create "C" <> ite ("A" /~? "C") (trans "C" ("A", "C")) (trans "C" ("B", "C"))

f51' :: ProbBellKATPolicy
f51' = create "C" <> ite ("B" /~? "C") (trans "C" ("B", "C")) (trans "C" ("A", "C"))

p51iii :: ProbBellKATPolicy
p51iii = e51' <||> f51'

p51iii' :: ProbBellKATPolicy
p51iii' = p51iii <> p51iii

p51nu1' :: D' (TaggedBellPairs BellKATTag)
p51nu1' = [(["A" ~ "C", "B" ~ "C"], 766228/1000000), (["A" ~ "C"], 201006/1000000), (["B" ~ "C"], 166374/10000000), ([], 16129/1000000)]

p51nu2' :: D' (TaggedBellPairs BellKATTag)
p51nu2' = [(["A" ~ "C", "B" ~ "C"], 766228/1000000), (["A" ~ "C"], 156654/1000000), (["B" ~ "C"], 443574/10000000), ([], 32761/1000000)]

p51nu3' :: D' (TaggedBellPairs BellKATTag)
p51nu3' = [(["A" ~ "C", "B" ~ "C"], 766228/1000000), (["A" ~ "C"], 182718/1000000), (["B" ~ "C"], 280674/10000000), ([], 22987/1000000)]

-- | == IV TODO: don't have star yet

p51iv :: Int -> ProbBellKATPolicy
p51iv n = whileN n ("A" /~? "C" ||* "B" /~? "C") p51iii

-- | = Example 5.3

p53pac :: ProbabilisticActionConfiguration
p53pac = PAC [] [] [(("A", "C"), 36/10000), (("B", "C"), 28/10000)] [("C", 32/1000)]

p53nc :: NetworkCapacity BellKATTag
p53nc = ["A" ~ "B", "A" ~ "C", "B" ~ "C"]

p53OneAttempt :: ProbBellKATPolicy
p53OneAttempt = 
    let n = 450 :: Int
     in (stimes n (ite ("A" /~? "C") (ucreate ("B", "C")) mempty) 
            <||> stimes n (ite ("B" /~? "C") (ucreate ("B", "C")) mempty)) 
        <> swap "C" ("A", "B")

p53 :: Int -> ProbBellKATPolicy 
p53 n = 
    whileN n ("A" /~? "C" ||* "B" /~? "C") p53OneAttempt

-- TODO: don't have star yet

-- | == Repater swap protocol

p53'e :: Int -> Int -> ProbBellKATPolicy
p53'e n k = 
    whileN n ("A" /~? "C") $
        (whileN k (hasNotSubset ["A" ~ "C", "A" ~ "C"])
            (create "C" <.> create "C") <> (trans "C" ("A", "C") <.> trans "C" ("A", "C")))
        <> distill ("A", "C")

p53'e' :: Int -> Int -> ProbBellKATPolicy
p53'e' n k = 
    whileN n ("B" /~? "C") $
        (whileN k (hasNotSubset ["B" ~ "C", "B" ~ "C"])
            (create "C" <.> create "C") <> (trans "C" ("B", "C") <.> trans "C" ("B", "C")))
        <> distill ("B", "C")

p53'nc :: NetworkCapacity BellKATTag
p53'nc = stimes 4 ["C" ~ "C"] <> stimes 2 ["A" ~ "C"] <> stimes 2 ["B" ~ "C"]

p53'pac :: ProbabilisticActionConfiguration
p53'pac = PAC 
    [(("C", "B"), 1 / 2),(("C", "A"), 4 / 5)]
    [("C", 9/10)]
    [(("A", "C"), 36/10000), (("B", "C"), 28/10000)]
    [("C", 32/1000)]

p53' :: Int -> Int -> ProbBellKATPolicy
p53' n k = (p53'e n k <||> p53'e' n k) <> swap "C" ("A", "B")

-- | = Auxiliary definitions

fromBasicAction :: CreatesBellPairs a BellKATTag => TaggedAction BellKATTag -> a
fromBasicAction = tryCreateBellPairFrom . simpleActionMeaning


pac :: ProbabilisticActionConfiguration
pac = PAC [(("C", "B"), 1 / 2),(("C", "A"), 4 / 5)] [("C", 2 / 3)] [] []

probActionMeaning :: TaggedAction t -> CreateBellPairArgs t
probActionMeaning = probabilisticActionMeaning pac

fromBasicActionP :: CreatesBellPairs a BellKATTag => TaggedAction BellKATTag -> a
fromBasicActionP = tryCreateBellPairFrom . probActionMeaning

asAutomaton :: ProbBellKATPolicy -> BellKATAutomaton
asAutomaton x = getGFA (meaning . mapDesugarActions simpleActionMeaning $ x)

asAutomatonP :: ProbBellKATPolicy -> BellKATAutomaton
asAutomatonP x = getGFA (meaning . mapDesugarActions probActionMeaning $ x)

spec :: Spec
spec = do
    describe "GuardedAutomatonStepQuantum" $ do
        it "correctly represents example 4.2 det (e)" $
            asAutomaton e42 `shouldBe` e42FA
        it "correctly represents example 4.2 det (f)" $
            asAutomaton f42 `shouldBe` f42FA
        it "correctly represents example 4.2 det (e || mempty)" $
            (e42FA <||> mempty) `shouldBe` e42FA
        it "correctly represents example 4.2 det (e || f)" $
            (e42FA <||> f42FA) `shouldBe` ef42FA
        it "correctly represents example 4.2 prob (e)" $
            asAutomatonP e42 `shouldBe` e42FAp
        it "correctly represents example 4.2 prob (e || f)" $
            asAutomatonP ef42 `shouldBe` ef42FAp
        it "prints example 5.1 II" $
            print $ asAutomatonP p51ii
        it "prints example 5.1 III" $
            print $ asAutomatonP p51iii
        it "prints example 5.1 IV (two interations)" $
            print $ asAutomatonP (p51iv 2)
    describe "Probabilistic policy meaning" $ do
        it "correctly computes example 4.2" $ do
            applyProbStarPolicy pac Nothing ef42 [] `shouldBe`
                [ [ ([]                     , 23/135)
                  , (["B" ~ "C"]           , 6/135)
                  , (["A" ~ "C"]           , 72/135)
                  , (["A" ~ "C", "B" ~ "C"], 24/135)
                  , (["C" ~ "C"]           , 10/135)
                  ]
                , [ ([]                    , 41/135)
                  , (["B" ~ "C"]           , 36/135)
                  , (["A" ~ "C"]           , 24/135)
                  , (["A" ~ "C", "B" ~ "C"], 24/135)
                  , (["C" ~ "C"]           , 10/135)
                  ]
                ]
        it "correctly handles example 5.1.I (parallel)" $ do
            applyProbStarPolicy p51pac (Just p51nc) p51i [] `shouldBe`
                [p51mu1, p51mu2]
        it "correctly handles example 5.1.I (ordered)" $ do
            applyProbStarPolicy p51pac (Just p51nc) p51i' [] `shouldBe`
                [p51mu1]
        it "prints system for example 5.1.I (parallel)" $ do
            print $ applyProbStarPolicySystem p51pac (Just p51nc) p51i []
        it "prints system for example 5.1.I (ordered)" $ do
            print $ applyProbStarPolicySystem p51pac (Just p51nc) p51i' []
        it "combines correctly sequential composition i 5.1.II (parallel)" $ do
            let k = applyProbStarPolicy p51pac (Just p51nc) p51i
            (k [] >>- k) `shouldBe` applyProbStarPolicy p51pac (Just p51nc) p51ii []
        it "prints system for example 5.1.II (parallel)" $ do
            print $ applyProbStarPolicySystem p51pac (Just p51nc) p51ii []
        it "has right generators for example 5.1.II (parallel)" $ do
            let cd = applyProbStarPolicy p51pac (Just p51nc) p51ii []
            C.getGenerators cd `shouldContain` [p51nu1]
            C.getGenerators cd `shouldContain` [p51nu2]
            p51nu3 `C.memberC` cd `shouldBe` True
        it "correctly handles example 5.1.II (parallel)" $ do
            applyProbStarPolicy p51pac (Just p51nc) p51ii [] `shouldBe`
                [p51nu1,p51nu2,p51nu3]
        it "correctly handles example 5.1.II (ordered)" $ do
            applyProbStarPolicy p51pac (Just p51nc) p51ii' [] `shouldBe`
                [p51nu1]
        it "correctly handles example 5.1.III" $ do
            applyProbStarPolicy p51pac (Just p51nc) p51iii [] `shouldBe`
                [p51nu1', p51nu2', p51nu3']
        it "does produce different results for 5.1.III and 5.1.II" $ 
            applyProbStarPolicy p51pac (Just p51nc) p51ii [] `shouldNotBe`
                applyProbStarPolicy p51pac (Just p51nc) p51iii' []
        it "does produce different results for one iteration of 5.1.II and 5.1.III on input B~C" $ do
            applyProbStarPolicy p51pac (Just p51nc) p51i ["B" ~ "C"]
            `shouldNotBe`
            applyProbStarPolicy p51pac (Just p51nc) p51iii ["B" ~ "C"]
        it "prints system of example 5.1.IV (two iterations)" $ do
            print $ applyProbStarPolicySystem p51pac (Just p51nc) (p51iv 2) []
        it "prints the probabilities of example 5.1.IV (3 iterations) [LONG]" $ do
            let result = applyProbStarPolicy' @_ @_ @Double p51pac (Just p51nc) (p51iv 3) []
            print result
            putStrLn $ "result size is " <> show (length $ C.getGenerators result)
        it "prints probabilities example 5.3 (Pompli, one attempt) [LONG]" $ do
            print $ applyProbStarPolicy' @_ @_ @Double p53pac (Just p53nc) p53OneAttempt []
        it "prints probabilities example 5.3 (Pompli) [LONG]" $ do
            print $ applyProbStarPolicy' @_ @_ @Double p53pac (Just p53nc) (p53 10) []
        it "prints system 5.3 (Coopmans)" $ do
            print $ applyProbStarPolicySystem p53'pac (Just p53'nc) (p53' 2 1) []
        it "prints probabilities example 5.3 (Coopmans)" $ do
            print $ applyProbStarPolicy p53'pac (Just p53'nc) (p53' 2 1) []
