{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLists        #-}

module BellKAT.DSL where

import           Data.Functor.Contravariant
import           Data.Default
import           Data.List.NonEmpty         (NonEmpty (..))

import qualified BellKAT.Utils.Multiset as Mset
import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Tests
import           BellKAT.Definitions.Policy

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

destroy :: DSLFunctions p => (Location, Location) -> p
destroy loc = defaultTagged $ Destroy loc

class DSLTestNeq t tag | t -> tag where
    hasNotSubset :: TaggedBellPairs tag -> t

(/~?) :: (Default tag, Ord tag, DSLTestNeq t tag) => Location -> Location -> t
l /~? l' = hasNotSubset [l ~ l']

class DSLTestNeq t tag => DSLTest t tag where
    hasSubset :: TaggedBellPairs tag -> t

(~~?) :: (Ord tag, Default tag, DSLTest t tag) => Location -> Location -> t
l ~~? l' = hasSubset [l ~ l']

instance Ord t => DSLTestNeq (BellPairsPredicate t) t where
    hasNotSubset x = BPsPredicate (not . (x `Mset.isSubsetOf`))

instance Ord t => DSLTest (BellPairsPredicate t) t where
    hasSubset x = BPsPredicate (x `Mset.isSubsetOf`)

instance Ord t => DSLTestNeq (FreeTest t) t where
    hasNotSubset x = FTNot $ FTSubset x

instance Ord t => DSLTest (BoundedTest t) t where
    hasSubset = boundedTestContains

instance Ord t => DSLTestNeq (BoundedTest t) t where
    hasNotSubset = boundedTestNotContains

instance Ord t => DSLTest (FreeTest t) t where
    hasSubset = FTSubset

class DSLFunctions p where
    defaultTagged :: Action -> p
    (.%) :: p -> DupKind -> p

instance DSLFunctions (TaggedAction (Maybe tag)) where
    defaultTagged a = TaggedAction def a def mempty
    (TaggedAction p a t _) .% dk = TaggedAction p a t dk

instance DSLFunctions (Simple Policy (Maybe tag)) where
    defaultTagged = APAtomic . defaultTagged

    APAtomic ta .% dk = APAtomic $ ta .% dk
    _ .% _ = error "cannot attach dup to this thing"

instance DSLFunctions (SeqWithTests Policy test (Maybe tag)) where
    defaultTagged = APAtomic . pure . AAction . defaultTagged

    APAtomic (AAction ta :| []) .% dk = APAtomic [ AAction $ ta .% dk ]
    _ .% _ = error "cannot attach dup to this thing"

instance DSLFunctions (Simple OrderedStarPolicy (Maybe tag)) where
    defaultTagged = OSPAtomic . defaultTagged 
    _ .% _ = error "cannot attach dup to this thing"

instance DSLFunctions (WithTests OrderedStarPolicy test (Maybe tag)) where
    defaultTagged = OSPAtomic . AAction . defaultTagged
    _ .% _ = error "cannot attach dup to this thing"

instance DSLFunctions (SeqWithTests FullPolicy test (Maybe tag)) where
    defaultTagged = FPAtomic . pure .  AAction . defaultTagged

    FPAtomic (AAction ta :| []) .% dk = FPAtomic [ AAction $ ta .% dk ]
    _ .% _ = error "cannot attach dup to this thing"

instance DSLFunctions (Simple (OrderedGuardedPolicy test) (Maybe tag)) where
    defaultTagged = OGPAtomic . defaultTagged
    _ .% _ = error "cannot attach dup to this thing"

infixl 7 <..>

class FakeOrderedSemigroup a where
    (<..>) :: a -> a -> a

instance FakeOrderedSemigroup (SeqWithTests Policy test tag) where
    (APAtomic tas) <..> (APAtomic tas') = APAtomic (tas <> tas')
    _ <..> _                            = error "Can only compose atomics with <.>"

instance FakeOrderedSemigroup (SeqWithTests FullPolicy test tag) where
    (FPAtomic tas) <..> (FPAtomic tas') = FPAtomic (tas <> tas')
    _ <..> _                            = error "Can only compose atomics with <.>"

instance FakeOrderedSemigroup (WithTests OrderedStarPolicy test tag) where
    (<..>) = OSPOrdered

instance FakeOrderedSemigroup (Simple OrderedStarPolicy tag) where
    p <..> p' = OSPOrdered p p'

dupA :: DupKind
dupA = DupKind { dupBefore = False, dupAfter = True }

dupB :: DupKind
dupB = DupKind { dupBefore = True, dupAfter = False }

infixl 9 .~

class Taggable a t | a -> t where
    (.~) :: a -> t -> a

instance Taggable (Simple Policy (Maybe t)) t where
    APAtomic (TaggedAction ti a _ dupKind) .~ t = APAtomic (TaggedAction ti a (Just t) dupKind)
    _ .~ _ = error "cannot attach tag to this thing"

instance Taggable (TaggedBellPair (Maybe t)) t where
    tbp .~ t = tbp { bellPairTag = Just t }

instance Taggable (Simple (OrderedGuardedPolicy test) (Maybe t)) t where
    OGPAtomic (TaggedAction ti a _ dupKind) .~ t = OGPAtomic (TaggedAction ti a (Just t) dupKind)
    _ .~ _ = error "cannot attach tag to this thing"

instance Eq t => Taggable (BellPairsPredicate (Maybe t)) t where
    t .~ tag = BPsPredicate $ \bps -> all (hasTag tag) bps && getBPsPredicate t bps

class InverseTaggable a t | a -> t where
    (~.) :: t -> a -> a


instance InverseTaggable (Simple Policy (Maybe t)) t where
    t ~. APAtomic (TaggedAction _ a to dupKind) = APAtomic (TaggedAction (Just t) a to dupKind)
    _ ~. _ = error "cannot attach tag to this thing"

instance InverseTaggable (Simple (OrderedGuardedPolicy test) (Maybe t)) t where
    t ~. OGPAtomic (TaggedAction _ a to dupKind)= OGPAtomic (TaggedAction (Just t) a to dupKind)
    _ ~. _ = error "cannot attach tag to this thing"

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
