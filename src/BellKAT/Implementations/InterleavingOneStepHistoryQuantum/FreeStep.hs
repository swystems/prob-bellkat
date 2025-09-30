module BellKAT.Implementations.InterleavingOneStepHistoryQuantum.FreeStep
    ( FreeStep
    , runFreeStep
    ) where

import           Data.Functor.Classes

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Tests
import           BellKAT.Definitions.Structures

data FreeStep test op tag = FSCreate (CreateBellPairArgs op tag) | FSTest (test tag)

runFreeStep
    :: (Test test, Ord tag, Tests a BellPairsPredicate tag, CreatesBellPairs a op tag)
    => FreeStep test op tag -> a
runFreeStep (FSCreate args) = tryCreateBellPairFrom args
runFreeStep (FSTest args) = test . toBPsPredicate $ args

instance Show1 test => Show1 (FreeStep test Probability) where
  liftShowsPrec _ _ _ (FSCreate ca)
    = showString "create"
        . (if cbpOp ca < 1.0 then showString "?" else id )
        -- TODO: below we lose tag information
        . showString "(" . shows (bellPair . cbpOutputBP $ ca). showString ")"
  liftShowsPrec s sl _ (FSTest t) = showString "[" . liftShowsPrec s sl 0 t . showString "]"

instance CreatesBellPairs (FreeStep test op t) op t where
  tryCreateBellPairFrom = FSCreate

instance Tests (FreeStep test op t) test t where
  test = FSTest

