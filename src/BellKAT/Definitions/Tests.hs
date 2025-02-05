module BellKAT.Definitions.Tests 
    (
    FreeTest(..),
    BellPairsPredicate(..),
    Test(..),
    RestrictedTest,
    createRestrictedTest,
    (.&&.),
    (.+.),
    ) where

import qualified Data.Multiset              as Mset
import           Data.Foldable              (toList)
import           Data.List                  (sort)
import           Data.Functor.Classes
import           Data.Default

import BellKAT.Definitions.Core

data FreeTest t
    = FTSubset (TaggedBellPairs t)
    | FTNot (FreeTest t)

instance Show1 FreeTest where
    liftShowsPrec s sl i (FTNot t) = showString "~" . liftShowsPrec s sl i t
    liftShowsPrec _ _ _ (FTSubset bps) = shows (map bellPair $ toList bps)

instance (Default t, Show t, Eq t) => Show (FreeTest t) where
    showsPrec _ (FTSubset x) = shows x
    showsPrec d (FTNot x) = showParen (app_prec < d) $ showString "not " . shows x
      where app_prec = 10

newtype BellPairsPredicate t = BPsPredicate { getBPsPredicate :: TaggedBellPairs t -> Bool }

-- | Class of things that can serve as `BellPairsPredicate`
class Test test where
    toBPsPredicate :: Ord tag => test tag -> BellPairsPredicate tag

instance Test FreeTest where
    toBPsPredicate (FTNot t) = BPsPredicate $ not . getBPsPredicate (toBPsPredicate t)
    toBPsPredicate (FTSubset bps) = BPsPredicate (bps `Mset.isSubsetOf`)

instance Test BellPairsPredicate where
    toBPsPredicate = id

instance Show (BellPairsPredicate t) where
  showsPrec _ _ = shows "test"


newtype RestrictedTest tag = RestrictedTest [TaggedBellPairs tag]
    deriving newtype (Eq, Ord)

-- | Constructs `RestrictedTest` in a normalized (non-redundant) form
createRestrictedTest :: Ord tag => [TaggedBellPairs tag] -> RestrictedTest tag
createRestrictedTest = RestrictedTest . sort . restrictedTestNormalize

restrictedTestNormalize :: Ord tag => [TaggedBellPairs tag] -> [TaggedBellPairs tag]
restrictedTestNormalize [] = []
restrictedTestNormalize (x : xs) =
    if any (`Mset.isSubsetOf` x) xs
        then restrictedTestNormalize xs
        else x : restrictedTestNormalize xs

instance (Show tag, Default tag, Eq tag) => Show (RestrictedTest tag) where
    showsPrec _ (RestrictedTest []) = showString "TRUE"
    showsPrec _ (RestrictedTest (x : xs)) = shows (toList x) . showsRest xs
      where
        showsRest [] = id
        showsRest (y : ys) = showString " /\\ " . shows (toList y) . showsRest ys

-- | Performs multiset union of each set in a `RestrictedTest` with the given `TaggedBellPairs`
(.+.) :: (Ord tag) => RestrictedTest tag -> TaggedBellPairs tag -> RestrictedTest tag
(RestrictedTest bpss) .+. bps = createRestrictedTest (map (<> bps) bpss)

-- | Performs logical /and/ of two `RestrictedTest`s
(.&&.) :: (Ord tag) => RestrictedTest tag -> RestrictedTest tag -> RestrictedTest tag
(RestrictedTest bpss) .&&. (RestrictedTest bpss') =
    createRestrictedTest (bpss <> bpss')

instance Test RestrictedTest where
    toBPsPredicate (RestrictedTest s) = BPsPredicate $ \bps -> not (any (`Mset.isSubsetOf` bps) s)
