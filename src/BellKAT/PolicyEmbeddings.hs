module BellKAT.PolicyEmbeddings where

import Data.List.NonEmpty (NonEmpty)

import BellKAT.Definitions.Core
import BellKAT.Definitions.Structures
import BellKAT.Definitions.Policy
import BellKAT.Utils.NonEmpty

class HasMeaning p a where
    meaning :: p -> a

instance Quantum a tag => HasMeaning (CreateBellPairArgs tag) a where
    meaning = tryCreateBellPairFrom

instance TestsOrderedLayeredQuantum a test tag 
        => HasMeaning (NonEmpty (Atomic CreateBellPairArgs test tag)) a where
    meaning ta = liftLayer $ foldNonEmpty (<.>) $ meaning <$> ta

instance (ParallelSemigroup a, Semigroup a, HasMeaning at a) => HasMeaning (Policy at) a where
    meaning (APAtomic ta)    = meaning ta
    meaning (APSequence p q) = meaning p <> meaning q
    meaning (APParallel p q) = meaning p <||> meaning q

    
instance {-# OVERLAPPABLE #-} (TestsOrderedQuantum a test tag) 
        => HasMeaning (Atomic CreateBellPairArgs test tag) a where
    meaning (AAction a) = tryCreateBellPairFrom a
    meaning (ATest t)   = test t

instance (TestsOrderedLayeredQuantum a test tag) => HasMeaning (Atomic CreateBellPairArgs test tag) (Layer a) where
    meaning (AAction a) = orderedTryCreateBellPairFrom a
    meaning (ATest t)   = orderedTest t

instance (HasMeaning (CreateBellPairArgs at) a, Semigroup a, ParallelSemigroup a, ChoiceSemigroup a) 
  => HasMeaning (OneRoundPolicy (CreateBellPairArgs at)) a where
    meaning (ORPAtomic ta) = meaning ta
    meaning (ORPSequence p q) = meaning p <> meaning q
    meaning (ORPParallel p q) = meaning p <||> meaning q
    meaning (ORPChoice p q) = meaning p <+> meaning q

instance (ChoiceSemigroup a, Monoid a, TestsOrderedLayeredQuantum a test tag) 
  => HasMeaning (FullPolicy (NonEmpty (Atomic CreateBellPairArgs test tag))) a where
    meaning (FPAtomic ta) = liftLayer $ foldNonEmpty (<.>) $ meaning <$> ta
    meaning (FPSequence p q) = meaning p <> meaning q
    meaning FPOne = mempty
    meaning (FPParallel p q) = meaning p <||> meaning q
    meaning (FPChoice p q) = meaning p <+> meaning q

instance (MonoidStar a, OrderedSemigroup a, TestsOrderedLayeredQuantum a test tag) 
  => HasMeaning (StarPolicy (NonEmpty (Atomic CreateBellPairArgs test tag))) a where
    meaning (SPAtomic ta) = liftLayer $ foldNonEmpty (<.>) $ meaning <$> ta
    meaning (SPSequence p q) = meaning p <> meaning q
    meaning SPOne = mempty
    meaning (SPStar p) = star (meaning p)
    meaning (SPParallel p q) = meaning p <||> meaning q
    meaning (SPChoice p q) = meaning p <+> meaning q

instance (MonoidStar a, OrderedSemigroup a, Quantum a tag) 
  => HasMeaning (OrderedStarPolicy (CreateBellPairArgs tag)) a where
    meaning (OSPAtomic ta) = meaning ta
    meaning (OSPOrdered p q) = meaning p <.> meaning q
    meaning (OSPSequence p q) = meaning p <> meaning q
    meaning OSPOne = mempty
    meaning (OSPStar p) = star (meaning p)
    meaning (OSPParallel p q) = meaning p <||> meaning q
    meaning (OSPChoice p q) = meaning p <+> meaning q

instance (MonoidStar a, OrderedSemigroup a, TestsOrderedQuantum a test tag) 
  => HasMeaning (OrderedStarPolicy (Atomic CreateBellPairArgs test tag)) a where
    meaning (OSPAtomic ta) = meaning ta
    meaning (OSPOrdered p q) = meaning p <.> meaning q
    meaning (OSPSequence p q) = meaning p <> meaning q
    meaning OSPOne = mempty
    meaning (OSPStar p) = star (meaning p)
    meaning (OSPParallel p q) = meaning p <||> meaning q
    meaning (OSPChoice p q) = meaning p <+> meaning q

instance (OrderedSemigroup a, Monoid a, ParallelSemigroup a, Guarded test a, Quantum a tag) 
  => HasMeaning (OrderedGuardedPolicy test (CreateBellPairArgs tag)) a where
    meaning OGPOne = mempty
    meaning (OGPAtomic ta) = meaning ta
    meaning (OGPOrdered p q) = meaning p <.> meaning q
    meaning (OGPSequence p q) = meaning p <> meaning q
    meaning (OGPParallel p q) = meaning p <||> meaning q
    meaning (OGPIfThenElse t p q) = ite t (meaning p) (meaning q)
