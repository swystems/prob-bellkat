module BellKAT.Implementations.InterleavingOneStepHistoryQuantum.FreeStep
    ( FreeStep
    , runFreeStep
    ) where

import           Data.Functor.Classes

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Tests
import           BellKAT.Definitions.Structures

data FreeStep test tag = FSCreate (CreateBellPairArgs tag) | FSTest (test tag)

runFreeStep
    :: (Test test, Ord tag, Tests a BellPairsPredicate tag, CreatesBellPairs a tag)
    => FreeStep test tag -> a
runFreeStep (FSCreate args) = tryCreateBellPairFrom args
runFreeStep (FSTest args) = test . toBPsPredicate $ args

instance Show1 test => Show1 (FreeStep test) where
  liftShowsPrec _ _ _ (FSCreate ca)
    = showString "create"
        . (if cbpProbability ca < 1.0 then showString "?" else id )
        -- TODO: below we lose tag information
        . showString "(" . shows (bellPair . cbpOutputBP $ ca). showString ")"
  liftShowsPrec s sl _ (FSTest t) = showString "[" . liftShowsPrec s sl 0 t . showString "]"

instance CreatesBellPairs (FreeStep test t) t where
  tryCreateBellPairFrom = FSCreate

instance Tests (FreeStep test t) test t where
  test = FSTest

