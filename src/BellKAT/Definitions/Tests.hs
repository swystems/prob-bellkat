{-# LANGUAGE StrictData #-}
module BellKAT.Definitions.Tests
    (
    FreeTest(..),
    BellPairsPredicate(..),
    Test(..),
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
    if any (`Mset.isSubsetOf` x) xs
        then restrictedTestNormalize xs
        else x : restrictedTestNormalize xs

instance (Show tag, Default tag, Eq tag) => Show (RestrictedTest tag) where
    showsPrec _ (RestrictedTest []) = showString "TRUE"
    showsPrec _ (RestrictedTest (x : xs)) = shows x . showsRest xs
      where
        showsRest [] = id
        showsRest (y : ys) = showString " ∧ " . shows y . showsRest ys

-- | Performs multiset union of each set in a `RestrictedTest` with the given `TaggedBellPairs`
(.+.) :: (Ord tag) => RestrictedTest tag -> TaggedBellPairs tag -> RestrictedTest tag
(RestrictedTest bpss) .+. bps = createRestrictedTest (map (<> bps) bpss)

-- | Performs logical /and/ of two `RestrictedTest`s
(.&&.) :: (Ord tag) => RestrictedTest tag -> RestrictedTest tag -> RestrictedTest tag
(RestrictedTest bpss) .&&. (RestrictedTest bpss') =
    createRestrictedTest (bpss <> bpss')

instance Test RestrictedTest where
    toBPsPredicate (RestrictedTest s) = BPsPredicate $ \bps -> not (any (`Mset.isSubsetOf` bps) s)

type Range = (Int, Maybe Int)
type Bounds tag = Map (TaggedBellPair tag) Range

rangeGreater :: Int -> Range
rangeGreater k = (k + 1, Nothing)

rangeNotGreater :: Int -> Range
rangeNotGreater k = (0, Just (k + 1))

newtype BoundedTest tag = BoundedTest [Bounds tag] deriving newtype (Eq)

instance (Show tag, Ord tag, Default tag, Eq tag) => Show (BoundedTest tag) where
    show (BoundedTest xs)
      | BoundedTest xs == true = "TRUE"
      | BoundedTest xs == false = "FALSE"
      | otherwise = intercalate "∨" $ map showBounds xs

showBounds ::(Show tag, Default tag, Eq tag) => Bounds tag -> String
showBounds = intercalate "∧" . map showBound . Map.toList

showBound :: (Show tag, Default tag, Eq tag) => (TaggedBellPair tag, Range) -> String
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
boundedTestContains bps = BoundedTest [Map.fromSet (\k -> rangeGreater (Mset.count k bps - 1)) $ Mset.toSet bps]

boundedTestNotContains :: Ord tag => TaggedBellPairs tag -> BoundedTest tag
boundedTestNotContains bps = BoundedTest $ 
    [Map.singleton k $ rangeNotGreater (v - 1) | (k, v) <- Map.toList . Mset.toCountMap $ bps]

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
    toBPsPredicate (BoundedTest xs) = BPsPredicate $ \x -> any (`testBounds` x) xs

testBounds :: Ord tag => Bounds tag -> TaggedBellPairs tag -> Bool
testBounds bs bps = Map.foldlWithKey (\acc bp rg -> acc && testRange bp rg bps) True bs

testRange :: Ord tag => TaggedBellPair tag -> Range -> TaggedBellPairs tag -> Bool
testRange bp (lb, ub) bps =
    let c = Mset.count bp bps
     in c >= lb && maybe True (c <) ub

