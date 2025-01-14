module BellKAT.Definitions.Structures.Basic
    ( ParallelSemigroup(..)
    , ChoiceSemigroup(..)
    , OrderedSemigroup(..)
    , MonoidStar(..)
    ) where

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

class OrderedSemigroup a where
    (<.>) :: a -> a -> a

class (ChoiceSemigroup a, Monoid a) => MonoidStar a where
    star :: a -> a

