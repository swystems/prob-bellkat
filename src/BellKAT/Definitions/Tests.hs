{-# LANGUAGE StrictData #-}
-- |
-- Module: BellKAT.Definitions.Tests
-- Description: A module defining different flavours of predicates over BellPairs.
--
-- A module defining different flavours of `Test`s, i.e., predicates over `TaggedBellPairs`.
module BellKAT.Definitions.Tests
    (
    -- * Basic definitions related to predicates over multisets of Bell pairs
    BellPairsPredicate(..),
    Test(..),
    -- * Concrete instances of tests
    FreeTest(..),
    RestrictedTest,
    createRestrictedTest,
    (.&&.),
    (.+.),
    BoundedTest,
    boundedTestSingleton,
    boundedTestContains,
    boundedTestNotContains,
    rangeGreater,
    rangeNotGreater,
    ) where

import           Data.Maybe
import           Data.Foldable              (toList, foldl')
import           Data.List                  (sort, intercalate)
import           Data.Functor.Classes
import           Data.Default
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Containers.ListUtils
import qualified GHC.Exts (IsList, Item, fromList, toList)

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Definitions.Structures.Basic
import BellKAT.Definitions.Core

-- | Very simplistic test only allowing checking for either presence or the absence of a given
-- multi-set of `TaggedBellPair`s. Instances are usually constructed via helpers inside @BellKAT.DSL@ module using
-- `BellKAT.DSL.DSLTest` and `BellKAT.DSL.DSLTestNeq` instances
data FreeTest t
    = FTSubset (TaggedBellPairs t)
    | FTNot (FreeTest t)

instance Show1 FreeTest where
    liftShowsPrec s sl i (FTNot t) = showString "~" . liftShowsPrec s sl i t
    liftShowsPrec _ _ _ (FTSubset (Mset.LMS (bps, _))) = shows (map bellPair $ toList bps)

instance (Default t, Show t, Eq t) => Show (FreeTest t) where
    showsPrec _ (FTSubset x) = shows x
    showsPrec d (FTNot x) = showParen (app_prec < d) $ showString "not " . shows x
      where app_prec = 10

-- | Predicate over `TaggedBellPairs` parameterized by the respective, essentially a wrapper over
-- @TaggedBellPairs t -> Bool@
newtype BellPairsPredicate t = BPsPredicate { getBPsPredicate :: TaggedBellPairs t -> Bool }

-- | Class of that can serve as `BellPairsPredicate`
class Test test where
    testBellPairs :: Ord tag => test tag -> TaggedBellPairs tag -> Bool

instance Test FreeTest where
    testBellPairs (FTNot t) = not . testBellPairs t
    testBellPairs (FTSubset bps) = (bps `Mset.isSubsetOf'`)

instance Test BellPairsPredicate where
    testBellPairs = getBPsPredicate

instance Show (BellPairsPredicate t) where
  showsPrec _ _ = shows "test"


-- | Test representing a conjunction of tests for absences of `TaggedBellPair`s. Most notably used
-- when defining atomic actions (see `BellKAT.Definitions.Atomic.AtomicAction` and
-- `BellKAT.Definitions.Atomic.ProbabilisticAtomicAction`)
newtype RestrictedTest tag = RestrictedTest [TaggedBellPairs tag]
    deriving newtype (Eq, Ord)

instance Ord tag => GHC.Exts.IsList (RestrictedTest tag) where
    type Item (RestrictedTest tag) = TaggedBellPairs tag
    fromList = createRestrictedTest
    toList (RestrictedTest ts) = ts

-- | Constructs `RestrictedTest` in a normalized (non-redundant) form
createRestrictedTest :: Ord tag => [TaggedBellPairs tag] -> RestrictedTest tag
createRestrictedTest = RestrictedTest . sort . restrictedTestNormalize

restrictedTestNormalize :: Ord tag => [TaggedBellPairs tag] -> [TaggedBellPairs tag]
restrictedTestNormalize [] = []
restrictedTestNormalize (x : xs) =
    if any (`Mset.isSubsetOf'` x) xs
        then restrictedTestNormalize xs
        else x : restrictedTestNormalize xs

instance (Show tag, Default tag, Eq tag) => Show (RestrictedTest tag) where
    showsPrec _ (RestrictedTest []) = showString "⊤"
    showsPrec _ (RestrictedTest (x : xs)) = shows x . showsRest xs
      where
        showsRest [] = id
        showsRest (y : ys) = showString "∧" . shows y . showsRest ys

-- | Performs multiset union of each set in a `RestrictedTest` with the given `TaggedBellPairs`
(.+.) :: (Ord tag) => RestrictedTest tag -> TaggedBellPairs tag -> RestrictedTest tag
(RestrictedTest bpss) .+. bps = createRestrictedTest (map (<> bps) bpss)

-- | Performs logical /and/ of two `RestrictedTest`s
(.&&.) :: (Ord tag) => RestrictedTest tag -> RestrictedTest tag -> RestrictedTest tag
(RestrictedTest bpss) .&&. (RestrictedTest bpss') =
    createRestrictedTest (bpss <> bpss')

instance Test RestrictedTest where
    testBellPairs (RestrictedTest s) bps = not (any (`Mset.isSubsetOf'` bps) s)

-- | Bound for the number of things from below and above, the upper bound may be absent
-- essentially meaning "infinity"
type Range = (Int, Maybe Int)
type Bounds tag = Map (TaggedBellPair tag) Range

rangeGreater :: Int -> Range
rangeGreater k = (k + 1, Nothing)

rangeNotGreater :: Int -> Range
rangeNotGreater k = (0, Just (k + 1))

-- | A test representing a disjunction of conjunctions (DNF), where atomic proposition bound the
-- number of specific `TaggedBellPair`s using `Range`
newtype BoundedTest tag = BoundedTest [Bounds tag] deriving newtype (Eq)

instance (Tag tag, Default tag) => Show (BoundedTest tag) where
    show (BoundedTest xs)
      | BoundedTest xs == true = "⊤"
      | BoundedTest xs == false = "⊥"
      | otherwise = intercalate "∨" $ map showBounds xs

showBounds ::(Tag tag, Default tag) => Bounds tag -> String
showBounds = intercalate "∧" . map showBound . Map.toList

showBound :: (Tag tag, Default tag) => (TaggedBellPair tag, Range) -> String
showBound (bp, (lb, ub)) =
    let lbs = case lb of
                0 -> Nothing
                1 -> Just $ show bp
                n -> Just $ show n <> "×" <> show bp
        ubs = case ub of
                Nothing -> Nothing
                Just 1 -> Just $ "¬" <> show bp
                Just n -> Just $ "¬" <> show n <> "×" <> show bp
     in intercalate "∧" $ catMaybes [lbs, ubs]

boundedTestSingleton :: TaggedBellPair tag -> Range -> BoundedTest tag
boundedTestSingleton bps r = BoundedTest [Map.singleton bps r]

boundedTestContains :: Ord tag => TaggedBellPairs tag -> BoundedTest tag
boundedTestContains bps = BoundedTest [Map.fromSet (\k -> rangeGreater (Mset.count' k bps - 1)) $ Mset.toSet' bps]

boundedTestNotContains :: Ord tag => TaggedBellPairs tag -> BoundedTest tag
boundedTestNotContains bps = BoundedTest $ 
    [Map.singleton k $ rangeNotGreater (v - 1) | (k, v) <- Map.toList . Mset.toCountMap' $ bps]

andRange :: Range -> Range -> Range
andRange (la, ua) (lb, ub) =
    let uab = case (ua, ub) of
                (Nothing, x) -> x
                (x, Nothing) -> x
                (Just x, Just y) -> Just (min x y)
     in (max la lb, uab)

isRangeEmpty :: Range -> Bool
isRangeEmpty (_, Nothing) = False
isRangeEmpty (l, Just u) = u <= l

andBounds :: Ord tag => Bounds tag -> Bounds tag -> Maybe (Bounds tag)
andBounds x y =
    let newBounds = Map.unionWith andRange x y
     in if any isRangeEmpty newBounds
           then Nothing
           else Just newBounds

notRange :: Range -> [Range]
notRange (0, Nothing) = error "TRIVIAL RANGE"
notRange (lu, Nothing) = [(0, Just lu)]
notRange (0, Just lu) = [(lu, Nothing)]
notRange (k, Just lu) = [(0, Just k), (lu, Nothing)]

notBounds :: Bounds tag -> [Bounds tag]
notBounds = concatMap (\(k, r) -> map (Map.singleton k) $ notRange r) . Map.toList

instance Ord tag => Boolean (BoundedTest tag) where
    true = BoundedTest [mempty]
    false = BoundedTest []
    notB (BoundedTest []) = BoundedTest [mempty]
    notB (BoundedTest xs) = foldl' (&&*) true . map (BoundedTest . notBounds) $ xs
    (BoundedTest xs) ||* (BoundedTest ys) =
        BoundedTest $ nubOrd (xs <> ys)
    (BoundedTest xs) &&* (BoundedTest ys) =
        BoundedTest $ nubOrd $ catMaybes $ andBounds <$> xs <*> ys

instance Ord tag => DecidableBoolean (BoundedTest tag) where
    isFalse = (== false)

instance Test BoundedTest where
    testBellPairs (BoundedTest xs) x = any (`testBounds` x) xs

testBounds :: Ord tag => Bounds tag -> TaggedBellPairs tag -> Bool
testBounds bs bps = Map.foldlWithKey (\acc bp rg -> acc && testRange bp rg bps) True bs

testRange :: Ord tag => TaggedBellPair tag -> Range -> TaggedBellPairs tag -> Bool
testRange bp (lb, ub) bps =
    let c = Mset.count' bp bps
     in c >= lb && maybe True (c <) ub

