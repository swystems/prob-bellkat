{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module BellKAT.Parser.Policy
    ( SurfacePolicy(..)
    ) where

import           Data.Text (Text)

import           BellKAT.Definitions.Policy
import           BellKAT.Definitions.Tests

-- | A type representing an augmented 'Action' for 'OrderedGuardedPolicy',
-- embedding additional policy structures like bounded repetition, finite loops,
-- and let bindings.
data SurfacePolicy t =
    -- | Embedded policy
    Primitive (OrderedGuardedPolicy (BoundedTest t) (SurfacePolicy t))
    -- | Bounded repetition: @repeat n p@.
    | Repeat Int (SurfacePolicy t)
    -- | Finite loop: @while n test p@.
    | WhileN Int (BoundedTest t) (SurfacePolicy t)
    -- | Let binding: @let x = p1 in p2@.
    | Let Text
        (SurfacePolicy t) -- ^ Policy bound to 'x'
        (SurfacePolicy t) -- ^ Policy where 'x' can be used
    -- | A variable reference, to be used in 'Let' bindings.
    | PolicyVariable Text

instance (Show t, Show (BoundedTest t)) => Show (SurfacePolicy t) where
    showsPrec _ (Primitive action) = shows action
    showsPrec _ (PolicyVariable x) = shows x
    showsPrec d (Repeat n p) = showParen (d > 7) $
        showsPrec 8 n . showString " * " . showsPrec 7 p
    showsPrec d (WhileN n t p) = showParen (d > 3) $
        showString "whileN " . showsPrec 4 n . showString " " . showsPrec 4 t . showString " do " . showsPrec 4 p
    showsPrec d (Let var p1 p2) = showParen (d > 0) $
        showString "let " . shows var . showString " = " . shows p1 . showString " in " . shows p2
