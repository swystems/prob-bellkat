module BellKAT.Definitions.Structures.Basic
    ( ParallelSemigroup(..)
    , ChoiceSemigroup(..)
    , OrderedSemigroup(..)
    , MonoidStar(..)
    , Guarded(..)
    , whileN
    , module Data.Boolean
    , module Relude.Extra.Map
    , DecidableBoolean(..)
    ) where

import Data.Boolean
import Relude.Extra.Map

-- parallel composition is left-associative and has lower precedence than `<>`
infixl 5 <||>


-- | Define alg structure `ParallelSemigroup` with `<>` inherited from
-- `Semigroup` and new `<||>` for parallel composition
class ParallelSemigroup a where
    (<||>) :: a -> a -> a

-- choice is left-associative and has lower precedence than `<||>` or `<>`
infixl 4 <+>

class ChoiceSemigroup a where
    (<+>) :: a -> a -> a

infixl 6 <.>

class OrderedSemigroup a where
    (<.>) :: a -> a -> a

class (ChoiceSemigroup a, Monoid a) => MonoidStar a where
    star :: a -> a

class Guarded t a | a -> t where
    ite :: t -> a -> a -> a

class Boolean a => DecidableBoolean a where
    isFalse :: a -> Bool

whileN :: (Monoid a, Guarded t a) => Int -> t -> a -> a
whileN 0 _ _ = mempty
whileN n t x = ite t (x <> whileN (n - 1) t x) mempty

