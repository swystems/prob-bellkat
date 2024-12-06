module BellKAT.ActionEmbeddings 
    ( simpleActionMeaning
    ) where

import BellKAT.Definitions.Policy
import BellKAT.Definitions.Core

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
