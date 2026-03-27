{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module BellKAT.Parser.SurfacePolicy
    ( SurfacePolicy(..)
    , desugarSurfacePolicy
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text, unpack)
import           Data.Default
import           Data.Semigroup

import           BellKAT.Definitions.Policy
import           BellKAT.Definitions.Tests
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Structures.Basic

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

-- | Desugars a 'SurfacePolicy' into an 'OrderedGuardedPolicy'.
-- This function resolves 'Repeat', 'WhileN', and 'Let' constructs.
desugarSurfacePolicy
    :: (Default t, Tag t)
    => SurfacePolicy t
    -> OrderedGuardedPolicy (BoundedTest t) Action
desugarSurfacePolicy = desugarSurfacePolicyWithEnv Map.empty

-- | Helper function for 'desugarSurfacePolicy' that carries an environment of policy bindings.
desugarSurfacePolicyWithEnv
    :: (Default t, Tag t)
    => Map Text (OrderedGuardedPolicy (BoundedTest t) Action)
    -> SurfacePolicy t
    -> OrderedGuardedPolicy (BoundedTest t) Action
desugarSurfacePolicyWithEnv env (Primitive p) = desugarSurfacePolicyWithEnv env =<< p
desugarSurfacePolicyWithEnv env (Repeat n p) = stimes n (desugarSurfacePolicyWithEnv env p)
desugarSurfacePolicyWithEnv env (WhileN n test p) = desugarWhileN n test (desugarSurfacePolicyWithEnv env p)
desugarSurfacePolicyWithEnv env (Let var p1 p2) =
    let desugaredP1 = desugarSurfacePolicyWithEnv env p1
    in desugarSurfacePolicyWithEnv (Map.insert var desugaredP1 env) p2
desugarSurfacePolicyWithEnv env (PolicyVariable x) =
    Map.findWithDefault (error $ "Unbound policy variable: " <> unpack x) x env

-- | Helper function to desugar a finite 'whileN' loop.
desugarWhileN
    :: (Default t, Tag t)
    => Int
    -> BoundedTest t
    -> OrderedGuardedPolicy (BoundedTest t) Action
    -> OrderedGuardedPolicy (BoundedTest t) Action
desugarWhileN 0 _ _ = mempty -- If n is 0, the loop does nothing
desugarWhileN n test policy = ite test (policy <> desugarWhileN (n-1) test policy) mempty
