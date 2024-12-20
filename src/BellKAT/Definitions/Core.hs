{-# LANGUAGE OverloadedLists #-}
module BellKAT.Definitions.Core (
    -- * Basic definitions
    Location,
    BellPair(..),
    CreateBellPairArgs(..),
    hasLocation,
    TaggedBellPair(..),
    TaggedBellPairs,
    (@), 
    TaggedRequiredRoots,
    History(..),
    DupKind(..),
    HasDupKinds(..),
    -- * Tests
    FreeTest(..),
    RestrictedTest,  
    createRestrictedTest,
    (.+.),
    (.&&.),
    BellPairsPredicate(..),
    Test(..),
    -- * Dup for nodes in `History`
    dupHistory,
    dupForest,
    dupHistoryN,
    processDup,
    -- * `History` choice utilities
    chooseKHistories,
    -- * Reexports
    Predicate(..),
    Partial(..),
    ) where

import           Data.Bifunctor             (bimap)
import           Data.Foldable              (toList)
import           Data.Functor.Contravariant (Predicate (..))
import           Data.Functor.Classes
import           Data.List                  (sort)
import           Data.Monoid                (Endo (..))
import qualified Data.Multiset              as Mset
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.String                (IsString)
import           Data.Default
import qualified GHC.Exts                   (IsList, Item, fromList, toList)

import           Data.Vector.Fixed          (Arity, VecList)
import qualified Data.Vector.Fixed          as FV
import           Test.QuickCheck            hiding (choose, (.&&.))
import           Data.Multiset              (Multiset)

import           BellKAT.Utils.Choice
import           BellKAT.Utils.UnorderedTree
--
-- * Type definitions

newtype Location = Location { name :: String } deriving newtype (Eq, Show, Ord, IsString)

-- | `:~:` is our symbol for entangled pair
data BellPair = Location :~: Location

infix 9 :~:

instance Show BellPair where
    show (l1 :~: l2) = name l1 <> "~" <> name l2

instance Eq BellPair where
    l1 :~: l2 == l1' :~: l2' = sort [l1, l2] == sort [l2', l1']

instance Ord BellPair where
    compare (l1 :~: l2) (l1' :~: l2') = compare (sort [l1, l2]) (sort [l1', l2'])

hasLocation :: Location -> BellPair -> Bool
hasLocation l (l1 :~: l2) = l == l1 || l == l2

-- | `BellPair` with an optional tag
data TaggedBellPair t = TaggedBellPair
    { bellPair    :: BellPair
    , bellPairTag :: t
    } deriving stock (Eq, Ord)

instance (Show t, Eq t, Default t) => Show (TaggedBellPair t) where
    showsPrec _ (TaggedBellPair bp t) 
        | t == def = shows bp
        | otherwise = shows bp . showString "/" . shows t

infix 8 @ -- less than of `(:~:)`

-- | Attaches tag to a Bell pair
(@) :: BellPair -> tag -> TaggedBellPair tag
(@) = TaggedBellPair

-- | `TaggedBellPairs` is a multiset of Bell pairs, each with a tag
type TaggedBellPairs tag = Multiset (TaggedBellPair tag)

-- | DupKind controls when the nodes in the histories are duplicated
data DupKind = DupKind { dupBefore :: Bool, dupAfter :: Bool } deriving stock (Eq)

class HasDupKinds a where
    modifyDupKinds :: (DupKind -> DupKind) -> a -> a

    setDupKinds :: DupKind -> a -> a
    setDupKinds dk = modifyDupKinds (const dk)

instance (HasDupKinds a, Functor f) => HasDupKinds (f a) where
    modifyDupKinds f = fmap (modifyDupKinds f)

instance Semigroup DupKind where
    (DupKind x y) <> (DupKind x' y') = DupKind (x || x') (y || y')

instance Monoid DupKind where
    mempty = DupKind False False

data CreateBellPairArgs tag = CreateBellPairArgs
    { cbpOutputBP    :: TaggedBellPair tag -- ^ a produced (output) `BellPair`
    , cbpInputBPs    :: [TaggedBellPair tag] -- ^ a multiset of required (input) `BellPair`s
    , cbpProbability :: Double -- ^ probability of failure for operations that may fail
    , cbpDup         :: DupKind
    }

instance HasDupKinds (CreateBellPairArgs tag) where
  setDupKinds dk cbp = cbp { cbpDup = dk }
  modifyDupKinds f cbp = cbp { cbpDup = f (cbpDup cbp) }

instance Show1 CreateBellPairArgs where
  liftShowsPrec _ _ _ _ = shows "cbp"

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

-- | Represents tests as a set of `TaggedBellPairs` that /each/ must not appear in the input
newtype RestrictedTest tag = RestrictedTest [TaggedBellPairs tag]
    deriving newtype (Eq, Ord)

-- | Constructs `RestrictedTest` in a normalized (non-redundant) form
createRestrictedTest :: Ord tag => [TaggedBellPairs tag] -> RestrictedTest tag
createRestrictedTest = RestrictedTest . sort . restrictedTestNormalize

restrictedTestNormalize :: Ord tag => [TaggedBellPairs tag] -> [TaggedBellPairs tag]
restrictedTestNormalize [] = []
restrictedTestNormalize (x:xs) = 
    if any (`Mset.isSubsetOf` x) xs 
       then restrictedTestNormalize xs
       else x:restrictedTestNormalize xs

instance (Show tag, Default tag, Eq tag) => Show (RestrictedTest tag) where
    showsPrec _ (RestrictedTest []) = showString "TRUE"
    showsPrec _ (RestrictedTest (x:xs)) = shows (toList x) . showsRest xs
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

newtype BellPairsPredicate t = BPsPredicate { getBPsPredicate :: TaggedBellPairs t -> Bool }

-- | Class of things that can serve as `BellPairsPredicate`
class Test test where
    toBPsPredicate :: Ord tag => test tag -> BellPairsPredicate tag

instance Test FreeTest where
    toBPsPredicate (FTNot t) = BPsPredicate $ not . getBPsPredicate (toBPsPredicate t)
    toBPsPredicate (FTSubset bps) = BPsPredicate (bps `Mset.isSubsetOf`)

instance Test BellPairsPredicate where
    toBPsPredicate = id

instance Test RestrictedTest where
    toBPsPredicate (RestrictedTest s) = BPsPredicate $ \bps -> not (any (`Mset.isSubsetOf` bps) s)

instance Show (BellPairsPredicate t) where
  showsPrec _ _ = shows "test"

-- * History of BellPairs

type TaggedRequiredRoots tag = [TaggedBellPair tag]

-- | `History` is a forest of `BellPair`s
newtype History t = History { getForest :: UForest (TaggedBellPair t) }
    deriving newtype (Semigroup, Monoid, Eq, Ord, Arbitrary)

instance (Ord t) => GHC.Exts.IsList (History t) where
    type Item (History t) = UTree (TaggedBellPair t)
    toList = toList . getForest
    fromList = History . GHC.Exts.fromList

instance (Ord t, Default t, Show t) => Show (History t) where
    show = show . GHC.Exts.toList


-- | choose f subhistories /non-deterministically/
chooseKHistories
    :: (Ord t, Arity n)
    => VecList n [TaggedRequiredRoots t]
    -> History t -> Set (VecList n (History t), History t)
chooseKHistories reqRoots (History ts) =
      Set.map (bimap (FV.map History) History) $ chooseKSubforests reqRoots ts

-- | Duplicating history
dupHistory :: Ord t => History t -> History t
dupHistory = History . dupForest . getForest

dupForest :: Ord t => UForest t -> UForest t
dupForest = Mset.map (\t -> Node (rootLabel t) [t])

-- | Duplicating history
dupHistoryN :: Ord t => Int -> History t -> History t
dupHistoryN n = appEndo . mconcat . replicate n $ Endo dupHistory

processDupAfter :: Ord a => Bool -> a -> UForest a -> UTree a
processDupAfter True x ts  = Node x [Node x ts]
processDupAfter False x ts = Node x ts

processDupBefore :: Ord a => Bool -> UForest a -> UForest a
processDupBefore True ts  = ts
processDupBefore False ts = foldMap subForest ts

processDup :: Ord a => DupKind -> a -> UForest a -> UTree a
processDup dk x = processDupAfter (dupAfter dk) x . processDupBefore (dupBefore dk)


-- * Testing definitions

instance Arbitrary Location where
    arbitrary = Location <$> growingElements [[c] | c <- ['A'..'Z']]

instance Arbitrary BellPair where
    arbitrary = (:~:) <$> arbitrary <*> arbitrary

instance Arbitrary DupKind where
    arbitrary = DupKind <$> arbitrary <*> arbitrary

instance (Arbitrary t) => Arbitrary (TaggedBellPair t) where
    arbitrary = TaggedBellPair <$> arbitrary <*> arbitrary
