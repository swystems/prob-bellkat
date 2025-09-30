{-# LANGUAGE ConstraintKinds #-}
module BellKAT.Definitions.Structures.Quantum
    ( Quantum
    , CreatesBellPairs(..)
    , CreatesBellPairs'
    , Tests(..)
    , TestsQuantum
    , TestsOrderedLayeredQuantum
    , OrderedQuantum
    , Layered(..)
    , TestsOrderedLayered(..)
    , OrderedLayeredQuantum(..)
    , TestsOrderedQuantum
    ) where

import           Data.Orphans ()

import           BellKAT.Definitions.Structures.Basic
import           BellKAT.Definitions.Core

class CreatesBellPairs a op tag | a -> tag where
    -- | embedding Bell pair creation represented by `CreateBellPairArgs` into the structure `a`
    tryCreateBellPairFrom :: CreateBellPairArgs op tag -> a

type CreatesBellPairs' a = CreatesBellPairs a Probability

-- | `Quantum` is a `ParallelSemigroup` with `BellPair` creation
-- `a` is the type of the carrier and `t` is a tag
class (Semigroup a, ParallelSemigroup a, CreatesBellPairs a op t) => Quantum a op t | a -> t where

class Tests a test tag | a -> tag, a -> test where
    test :: test tag -> a

class (Quantum a op tag, Tests a test tag) => TestsQuantum a test op tag | a -> test, a -> tag where

class (Quantum a op tag, OrderedSemigroup a) => OrderedQuantum a op tag

-- | Some variations of `Quantum` that hve two domains: 
--
--  * `a` for the top-level behavior having `Semigroup` and `ParallelSemigroup` structures)
--
--  * `Layer a` for the one-layer behavior having `OrderedSemigroup` structure
class Layered a where
    data Layer a
    liftLayer :: Layer a -> a

-- | `Quantum` with two domains plus the test
class (Semigroup a, ParallelSemigroup a, Layered a, OrderedSemigroup (Layer a)) 
        => OrderedLayeredQuantum a op t | a -> t where
    orderedTryCreateBellPairFrom :: CreateBellPairArgs op t -> Layer a

class Layered a =>TestsOrderedLayered a test tag where
    orderedTest :: test tag -> Layer a

class (TestsOrderedLayered a test tag, OrderedLayeredQuantum a op tag) 
        => TestsOrderedLayeredQuantum a test op tag where

class (Tests a test tag, OrderedQuantum a op tag) => TestsOrderedQuantum a test op tag where
