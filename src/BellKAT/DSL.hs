{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLists        #-}

module BellKAT.DSL where

import           Data.Functor.Contravariant
import           Data.Default
import           Data.List.NonEmpty         (NonEmpty (..))

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Policy
import           BellKAT.Utils.UnorderedTree         (UTree (..))

distill :: DSLFunctions p => (Location, Location) -> p
distill locs = defaultTagged $ Distill locs

trans :: DSLFunctions p => Location -> (Location, Location) -> p
trans loc locs = defaultTagged $ Transmit loc locs

swap :: DSLFunctions p => Location -> (Location, Location) -> p
swap loc locs = defaultTagged $ Swap loc locs

create :: DSLFunctions p => Location -> p
create loc = defaultTagged $ Create loc

ucreate :: DSLFunctions p => (Location, Location) -> p
ucreate loc = defaultTagged $ UnstableCreate loc

(~) :: Default tag => Location -> Location -> TaggedBellPair tag
l1 ~ l2 = TaggedBellPair (l1 :~: l2) def

class DSLTestNeq t where
    (/~?) :: Location -> Location -> t

class DSLTestNeq t => DSLTest t where
    (~~?) :: Location -> Location -> t

instance DSLTestNeq (BellPairsPredicate (Maybe t)) where
    l /~? l' = BPsPredicate $ not . any ((== (l :~: l')) . bellPair)

instance DSLTest (BellPairsPredicate (Maybe t)) where
    l ~~? l' = BPsPredicate $ any $ (== (l :~: l')) . bellPair

instance Ord t => DSLTestNeq (FreeTest (Maybe t)) where
    l /~? l' = FTNot $ FTSubset [TaggedBellPair (l :~: l') Nothing]

instance Ord t => DSLTest (FreeTest (Maybe t)) where
    l ~~? l' = FTSubset [TaggedBellPair (l :~: l') Nothing]

class DSLFunctions p where
    defaultTagged :: Action -> p
    (.%) :: p -> DupKind -> p

class DSLTestFunctions p test t | p -> t, p -> test where
    test :: test (Maybe t) -> p

-- test :: Test (Maybe t) -> Ordered Policy (Maybe t)
instance DSLTestFunctions (SeqWithTests Policy test (Maybe tag)) test tag where
    test t = APAtomic [ ATest t ]

instance DSLTestFunctions (SeqWithTests FullPolicy test (Maybe tag)) test tag where
    test t = FPAtomic [ ATest t ]

instance DSLTestFunctions (SeqWithTests OrderedStarPolicy test (Maybe tag)) test tag where
    test t = OSPAtomic [ ATest t ]

instance DSLTestFunctions (WithTests OrderedStarPolicy test (Maybe tag)) test tag where
    test t = OSPAtomic (ATest t)

instance DSLFunctions (Simple Policy (Maybe tag)) where
    defaultTagged a = APAtomic $ TaggedAction def a Nothing mempty

    APAtomic (TaggedAction p a t _) .% dk = APAtomic $ TaggedAction p a t dk
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (SeqWithTests Policy test (Maybe tag)) where
    defaultTagged a = APAtomic [ AAction (TaggedAction def a Nothing mempty) ]

    APAtomic (AAction (TaggedAction p a t _) :| []) .% dk = APAtomic [ AAction (TaggedAction p a t dk) ]
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (Simple OrderedStarPolicy (Maybe tag)) where
    defaultTagged a = OSPAtomic $ TaggedAction def a Nothing mempty
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (WithTests OrderedStarPolicy test (Maybe tag)) where
    defaultTagged a = OSPAtomic $ AAction $ TaggedAction def a Nothing mempty
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (SeqWithTests FullPolicy test (Maybe tag)) where
    defaultTagged a = FPAtomic [ AAction (TaggedAction def a Nothing mempty) ]

    FPAtomic (AAction (TaggedAction p a t _) :| []) .% dk = FPAtomic [ AAction (TaggedAction p a t dk) ]
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (SeqWithTests OrderedStarPolicy test (Maybe tag)) where
    defaultTagged a = OSPAtomic [ AAction (TaggedAction def a Nothing mempty) ]

    OSPAtomic (AAction (TaggedAction p a t _) :| []) .% dk = OSPAtomic [ AAction (TaggedAction p a t dk) ]
    _ .% _                                = error "cannot attach dup to this thing"

instance DSLFunctions (SeqWithTests OrderedStarPolicy test ()) where
    defaultTagged a = OSPAtomic [ AAction (TaggedAction mempty a () mempty) ]

    OSPAtomic (AAction (TaggedAction p a t _) :| []) .% dk = OSPAtomic [ AAction (TaggedAction p a t dk) ]
    _ .% _                                = error "cannot attach dup to this thing"

infixl 7 <.>

class DSLOrderedSemigroup a where
    (<.>) :: a -> a -> a

instance DSLOrderedSemigroup (SeqWithTests Policy test tag) where
    (APAtomic tas) <.> (APAtomic tas') = APAtomic (tas <> tas')
    _ <.> _                            = error "Can only compose atomics with <.>"

instance DSLOrderedSemigroup (SeqWithTests FullPolicy test tag) where
    (FPAtomic tas) <.> (FPAtomic tas') = FPAtomic (tas <> tas')
    _ <.> _                            = error "Can only compose atomics with <.>"

instance DSLOrderedSemigroup (SeqWithTests OrderedStarPolicy test tag) where
    (OSPAtomic tas) <.> (OSPAtomic tas') = OSPAtomic (tas <> tas')
    _ <.> _                            = error "Can only compose atomics with <.>"

instance DSLOrderedSemigroup (WithTests OrderedStarPolicy test tag) where
    (<.>) = OSPOrdered

instance DSLOrderedSemigroup (Simple OrderedStarPolicy tag) where
    p <.> p' = OSPOrdered p p'

dupA :: DupKind
dupA = DupKind { dupBefore = False, dupAfter = True }

dupB :: DupKind
dupB = DupKind { dupBefore = True, dupAfter = False }

class Taggable a t | a -> t where
    (.~) :: a -> t -> a

instance Taggable (Simple Policy (Maybe t)) t where
    APAtomic (TaggedAction p a _ dupKind) .~ t = APAtomic (TaggedAction p a (Just t) dupKind)
    p .~ _                           = p

instance (Eq tag, Taggable (test (Maybe tag)) tag) 
        => Taggable (SeqWithTests OrderedStarPolicy test (Maybe tag)) tag where
    OSPAtomic (AAction (TaggedAction p a _ dupKind) :| []) .~ t = 
        OSPAtomic $ AAction (TaggedAction p a (Just t) dupKind) :| []
    OSPAtomic (ATest t :| []) .~ tag = 
        OSPAtomic $ ATest (t .~ tag) :| []
    p .~ _                           = p

instance Eq t => Taggable (BellPairsPredicate (Maybe t)) t where
    t .~ tag = BPsPredicate $ \bps -> all (hasTag tag) bps && getBPsPredicate t bps

hasTag :: Eq t => t -> TaggedBellPair (Maybe t) -> Bool
hasTag tag tbp = Just tag == bellPairTag tbp

instance Taggable (UTree (TaggedBellPair (Maybe t))) t where
    Node (TaggedBellPair bp _) ts .~ t = Node (TaggedBellPair bp (Just t)) ts

orP :: Predicate t -> Predicate t -> Predicate t
orP (Predicate f) (Predicate g) = Predicate ((||) <$> f <*> g)

(?~) :: t -> Simple Policy (Maybe t) -> Simple Policy (Maybe t)
tIn ?~ APAtomic (TaggedAction _ a tOut dupKind) = 
    APAtomic $ TaggedAction (Just tIn) a tOut dupKind
_ ?~ p                           = p


node :: Ord t => BellPair -> UTree (TaggedBellPair (Maybe t))
node bp = Node (TaggedBellPair bp Nothing) []
