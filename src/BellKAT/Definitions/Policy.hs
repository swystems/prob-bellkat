{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLists #-}
module BellKAT.Definitions.Policy where

import           Test.QuickCheck            hiding (choose)
import           Data.List.NonEmpty         (NonEmpty)

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Structures

-- * Main policy definitions

-- | Define primitive actions
data Action
    = Swap Location (Location, Location)
    | Transmit Location (Location, Location)
    | Distill (Location, Location)
    | Create Location
    | Destroy (Location, Location)
    | UnstableCreate (Location, Location)
    deriving stock (Show)

data TaggedAction t = TaggedAction
    { taTagIn  :: t
    , taAction :: Action
    , taTagOut :: t
    , taDup    :: DupKind
    }

instance Show t => Show (TaggedAction t) where
    show ta = "_:" <> show (taAction ta) <> ":" <> show (taTagOut ta)

instance {-# OVERLAPPING #-} HasDupKinds (TaggedAction t) where
    modifyDupKinds f ta = ta { taDup = f (taDup ta) }

-- | Define a language for policies, where `a` is the type of an atomic action
data Policy a
    = APAtomic a
    | APSequence (Policy a) (Policy a)
    | APParallel (Policy a) (Policy a)
    deriving stock (Show, Functor)

data OneRoundPolicy a
    = ORPAtomic a
    | ORPSequence (OneRoundPolicy a) (OneRoundPolicy a)
    | ORPParallel (OneRoundPolicy a) (OneRoundPolicy a)
    | ORPChoice (OneRoundPolicy a) (OneRoundPolicy a)
    deriving stock Functor

instance Show a => Show (OneRoundPolicy a) where
    showsPrec _ (ORPAtomic x) = shows x
    showsPrec d (ORPParallel x y) = showParen (d > 5) $ 
        showsPrec 6 x . showString " <||> " . showsPrec 6 y
    showsPrec d (ORPSequence x y) = showParen (d > 6) $ 
        showsPrec 7 x . showString " <> " . showsPrec 7 y
    showsPrec d (ORPChoice x y) = showParen (d > 4) $ 
        showsPrec 5 x . showString " <+> " . showsPrec 5 y

data FullPolicy a
    = FPAtomic a
    | FPSequence (FullPolicy a) (FullPolicy a)
    | FPParallel (FullPolicy a) (FullPolicy a)
    | FPOne
    | FPChoice (FullPolicy a) (FullPolicy a)
    deriving stock (Show, Functor)

data StarPolicy a
    = SPAtomic a
    | SPSequence (StarPolicy a) (StarPolicy a)
    | SPParallel (StarPolicy a) (StarPolicy a)
    | SPOne
    | SPStar (StarPolicy a)
    | SPChoice (StarPolicy a) (StarPolicy a)
    deriving stock (Show, Functor)

data OrderedStarPolicy a
    = OSPAtomic a
    | OSPSequence (OrderedStarPolicy a) (OrderedStarPolicy a)
    | OSPParallel (OrderedStarPolicy a) (OrderedStarPolicy a)
    | OSPOrdered (OrderedStarPolicy a) (OrderedStarPolicy a)
    | OSPOne
    | OSPStar (OrderedStarPolicy a)
    | OSPChoice (OrderedStarPolicy a) (OrderedStarPolicy a)
    deriving stock (Show, Functor)

data OrderedGuardedPolicy t a
    = OGPAtomic a
    | OGPSequence (OrderedGuardedPolicy t a) (OrderedGuardedPolicy t a)
    | OGPParallel (OrderedGuardedPolicy t a) (OrderedGuardedPolicy t a)
    | OGPOrdered (OrderedGuardedPolicy t a) (OrderedGuardedPolicy t a)
    | OGPIfThenElse t (OrderedGuardedPolicy t a) (OrderedGuardedPolicy t a)
    | OGPOne
    deriving stock (Functor)

instance Semigroup (Policy a) where
    (<>) = APSequence

instance Semigroup (FullPolicy a) where
    (<>) = FPSequence

instance ParallelSemigroup (FullPolicy a) where
    (<||>) = FPParallel

instance Monoid (FullPolicy a) where
    mempty = FPOne

instance ParallelSemigroup (Policy a) where
    (<||>) = APParallel

instance ChoiceSemigroup (OneRoundPolicy a) where
    (<+>) = ORPChoice

instance Semigroup (OneRoundPolicy a) where
    (<>) = ORPSequence

instance ParallelSemigroup (OneRoundPolicy a) where
    (<||>) = ORPParallel

instance ChoiceSemigroup (FullPolicy a) where
    (<+>) = FPChoice

instance Semigroup (OrderedStarPolicy a) where
    (<>) = OSPSequence

instance Monoid (OrderedStarPolicy a) where
    mempty = OSPOne

instance MonoidStar (OrderedStarPolicy a) where
    star = OSPStar

instance ParallelSemigroup (OrderedStarPolicy a) where
    (<||>) = OSPParallel

instance ChoiceSemigroup (OrderedStarPolicy a) where
    (<+>) = OSPChoice

instance OrderedSemigroup (OrderedStarPolicy a) where
    (<.>) = OSPOrdered

instance Semigroup (OrderedGuardedPolicy t a) where
    (<>) = OGPSequence

instance Monoid (OrderedGuardedPolicy t a) where
    mempty = OGPOne

instance ParallelSemigroup (OrderedGuardedPolicy t a) where
    (<||>) = OGPParallel

instance OrderedSemigroup (OrderedGuardedPolicy t a) where
    (<.>) = OGPOrdered

instance Guarded t (OrderedGuardedPolicy t a) where
    ite = OGPIfThenElse

data Atomic act test tag = AAction (act tag) | ATest (test tag)
    deriving stock (Show)

instance {-# OVERLAPPING #-} HasDupKinds (act tag) => HasDupKinds (Atomic act test tag) where
    modifyDupKinds f (AAction ta) = AAction (modifyDupKinds f ta)
    modifyDupKinds _ (ATest t) = ATest t
--
-- | Given a policy structure `p` and a `BellPair` tag `t` returns a policy with corresponding 
-- actions by supplying the default action structure
type Simple p tag = p (TaggedAction tag)

-- | Given a policy structure `p`, test structure `test` and a `BellPair` tag `t` returns a policy 
-- by supplying default action structure and using the supplied test structure `test` 
type WithTests p test tag = p (Atomic TaggedAction test tag)
--
-- | Given a policy structure `p`, test structure `test` and a `BellPair` tag `t` returns a policy
-- that internally prepresents ordered composition as a sequence of underlying actions
type SeqWithTests p test tag = p (NonEmpty (Atomic TaggedAction test tag))

instance Tests (SeqWithTests Policy test tag) test tag where
    test t = APAtomic [ ATest t ]

instance Tests (SeqWithTests FullPolicy test tag) test tag where
    test t = FPAtomic [ ATest t ]

instance Tests (WithTests OrderedStarPolicy test tag) test tag where
    test t = OSPAtomic (ATest t)

-- * Testing definitions

instance Arbitrary Action where
    arbitrary = oneof
        [ Swap <$> arbitrary <*> arbitrary
        , Create <$> arbitrary
        , Transmit <$> arbitrary <*> arbitrary
        , Distill <$> arbitrary
        ]

instance (Arbitrary t, Eq t) => Arbitrary (TaggedAction t) where
  arbitrary = do
    predicate <- arbitrary
    TaggedAction predicate <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Policy a) where
    arbitrary = do
        n <- getSize
        if n == 0 then
            resize 1 $ APAtomic <$> arbitrary
        else
            resize (n - 1) $ oneof [
                APSequence <$> arbitrary <*> arbitrary,
                APParallel <$> arbitrary <*> arbitrary
            ]

    shrink (APSequence p q) = [p, q]
    shrink (APParallel p q) = [p, q]
    shrink _                = []

instance (Arbitrary a) => Arbitrary (OneRoundPolicy a) where
    arbitrary = do
        n <- getSize
        if n == 0 then
            resize 1 $ ORPAtomic <$> arbitrary
        else
            resize (n - 1) $ oneof [
                ORPSequence <$> arbitrary <*> arbitrary,
                ORPParallel <$> arbitrary <*> arbitrary
            ]
    shrink (ORPSequence p q) = [p, q]
    shrink (ORPParallel p q) = [p, q]
    shrink _                = []
