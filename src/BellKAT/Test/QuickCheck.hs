{-# OPTIONS_GHC -Wno-missing-signatures #-}
module BellKAT.Test.QuickCheck where

import           Test.QuickCheck

import           Data.List         (intercalate)
import           Data.Set          (Set)
import           Data.Default

import           BellKAT.Definitions
import           BellKAT.Bundles.HistoryBased
import           BellKAT.Drawing



type TestTag = Int

testEquality 
    :: (Show t, Eq t, Default t)
    => (Simple p t -> History t -> Set (History t))
    -> Simple p t -> Simple p t -> History t -> Property
testEquality apply p q h =
    let hsP = apply p h
        hsQ = apply q h

        counterexampleText = intercalate "\n" $
               [drawHistoriesText hsP]
            <> ["   =/=   "]
            <> [drawHistoriesText hsQ]
     in counterexample counterexampleText (hsP == hsQ)


(~=~) :: Simple Policy TestTag -> Simple Policy TestTag -> History TestTag -> Property
(~=~) = testEquality applyPolicy

isAssociative :: (a -> a -> p) -> (a -> a -> a) -> a -> a -> a -> p
isAssociative (-~-) (-*-) p q s = ((p -*- q) -*- s) -~- (p -*- (q -*- s))

isCommutative :: (a -> a -> p) -> (a -> a -> a) -> a -> a -> p
isCommutative (-~-) (-*-) p q = (q -*- p) -~- (p -*- q)

distributesOver :: (a -> a -> p) -> (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> p
distributesOver (-~-) (-*-) (-+-) p q s = ((p -+- q) -*- s) -~- ((p -*- s) -+- (q -*- s))

sequentialCompositionIsAssociative = isAssociative (~=~) (<>)
parallelCompositionIsAssociative = isAssociative (~=~) (<||>)
parallelCompositionIsCommutative = isCommutative (~=~) (<||>)
sequentialCompositionDistributes = distributesOver (~=~) (<>) (<||>)

(~~) :: Simple Policy TestTag -> Simple Policy TestTag -> History TestTag -> Property
(~~) = testEquality applyPolicyTimely

timelySequentialCompositionIsAssociative = isAssociative (~~) (<>)
timelyParallelCompositionIsAssociative = isAssociative (~~) (<||>)
timelyParallelCompositionIsCommutative = isCommutative (~~) (<||>)
timelySequentialCompositionDistributes = distributesOver (~~) (<>) (<||>)

(~~~) :: Simple Policy TestTag -> Simple Policy TestTag -> History TestTag -> Property
(~~~) = testEquality applyPolicySteps

(~*~) :: Simple OneRoundPolicy TestTag -> Simple OneRoundPolicy TestTag -> History TestTag -> Property
(~*~) = testEquality applyOneStepPolicy

stepsSequentialCompositionIsAssociative = isAssociative (~~~) (<>)
stepsParallelCompositionIsAssociative = isAssociative (~~~) (<||>)
stepsParallelCompositionIsCommutative = isCommutative (~~~) (<||>)
stepsSequentialCompositionDistributes = distributesOver (~~~) (<>) (<||>)

bellkatQuickCheck :: Testable prop => prop -> IO ()
bellkatQuickCheck = quickCheckWith (stdArgs { maxSize = 4, maxSuccess = 10000 })
