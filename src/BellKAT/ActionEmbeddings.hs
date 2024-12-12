module BellKAT.ActionEmbeddings 
    ( simpleActionMeaning
    , CanDesugarActions(..)
    , mapDesugarActions
    ) where

import Data.Kind
import Data.List.NonEmpty (NonEmpty)

import BellKAT.Definitions.Policy
import BellKAT.Definitions.Core

class CanDesugarActions a where
    type Tag a :: Type
    type Desugared a :: Type
    desugarActions :: (TaggedAction (Tag a) -> CreateBellPairArgs (Tag a)) -> a -> Desugared a

mapDesugarActions 
    :: (Functor f, CanDesugarActions a)
    => (TaggedAction (Tag a) -> CreateBellPairArgs (Tag a)) -> f a -> f (Desugared a)
mapDesugarActions = fmap . desugarActions

simpleActionMeaning :: TaggedAction t -> CreateBellPairArgs t
simpleActionMeaning ta = case taAction ta of
    (Swap l (l1, l2))     -> CreateBellPairArgs
        (l1 :~: l2 @ taTagIn ta) [l :~: l1 @ taTagOut ta, l :~: l2 @ taTagOut ta]
        1.0 (taDup ta)
    (Transmit l (l1, l2)) -> CreateBellPairArgs
        (l1 :~: l2 @ taTagIn ta) [l :~: l @ taTagOut ta]
        1.0 (taDup ta)
    (Create l)            -> CreateBellPairArgs
        (l :~: l @ taTagIn ta ) [] 1.0 (taDup ta)
    (Distill (l1, l2))    -> CreateBellPairArgs
        (l1 :~: l2 @ taTagIn ta ) [l1 :~: l2 @ taTagOut ta, l1 :~: l2  @ taTagOut ta] 
        0.5 (taDup ta)
    (UnstableCreate (l1, l2)) -> CreateBellPairArgs
        (l1 :~: l2 @ taTagIn ta ) [] 0.5 (taDup ta)

instance CanDesugarActions (TaggedAction tag) where
    type Tag (TaggedAction tag) = tag
    type Desugared (TaggedAction tag) = CreateBellPairArgs tag
    desugarActions = id

instance CanDesugarActions a => CanDesugarActions (NonEmpty a) where
    type Tag (NonEmpty a) = Tag a
    type Desugared (NonEmpty a) = NonEmpty (Desugared a)
    desugarActions = fmap . desugarActions

instance CanDesugarActions (Atomic TaggedAction test tag) where 
    type Tag (Atomic TaggedAction test tag) = tag
    type Desugared (Atomic TaggedAction test tag) = (Atomic CreateBellPairArgs test tag)
    desugarActions f (AAction x) = AAction (f x)
    desugarActions _ (ATest t) = ATest t
