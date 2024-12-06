module BellKAT.ActionEmbeddings 
    ( simpleActionMeaning
    , CanDesugarActions(..)
    ) where

import Data.Kind
import Data.List.NonEmpty (NonEmpty)

import BellKAT.Definitions.Policy
import BellKAT.Definitions.Core

class CanDesugarActions a where
    type Tag a :: Type
    type Desugared a :: Type
    desugarActions :: (TaggedAction (Tag a) -> CreateBellPairArgs (Tag a)) -> a -> Desugared a

simpleActionMeaning :: TaggedAction t -> CreateBellPairArgs t
simpleActionMeaning ta = case taAction ta of
    (Swap l (l1, l2))     -> CreateBellPairArgs (taTagPredicate ta)
        (l1 :~: l2) [l :~: l1, l :~: l2] Nothing (taTag ta) (taDup ta)
    (Transmit l (l1, l2)) -> CreateBellPairArgs (taTagPredicate ta)
        (l1 :~: l2) [l :~: l] Nothing (taTag ta) (taDup ta)
    (Create l)            -> CreateBellPairArgs (taTagPredicate ta)
        (l :~: l) [] Nothing (taTag ta) (taDup ta)
    (Distill (l1, l2))    -> CreateBellPairArgs (taTagPredicate ta)
        (l1 :~: l2) [l1 :~: l2, l1 :~: l2] (Just 0.5) (taTag ta) (taDup ta)
    (UnstableCreate (l1, l2))            -> CreateBellPairArgs (taTagPredicate ta)
        (l1 :~: l2) [] (Just 0.5) (taTag ta) (taDup ta)

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

instance CanDesugarActions a => CanDesugarActions (Policy a) where
    type Tag (Policy a) = Tag a
    type Desugared (Policy a) = Policy (Desugared a)
    desugarActions = fmap . desugarActions

instance CanDesugarActions a => CanDesugarActions (FullPolicy a) where
    type Tag (FullPolicy a) = Tag a
    type Desugared (FullPolicy a) = FullPolicy (Desugared a)
    desugarActions = fmap . desugarActions

instance CanDesugarActions a => CanDesugarActions (StarPolicy a) where
    type Tag (StarPolicy a) = Tag a
    type Desugared (StarPolicy a) = StarPolicy (Desugared a)
    desugarActions = fmap . desugarActions

instance CanDesugarActions a => CanDesugarActions (OneRoundPolicy a) where
    type Tag (OneRoundPolicy a) = Tag a
    type Desugared (OneRoundPolicy a) = OneRoundPolicy (Desugared a)
    desugarActions = fmap . desugarActions
