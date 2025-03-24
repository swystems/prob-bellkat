{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData      #-}
module BellKAT.Definitions.Core (
    -- * Basic definitions
    Location(..),
    BellPair,
    LikeBellPair(..),
    BellPairs,
    Probability,
    CreateBellPairArgs(..),
    hasLocation,
    TaggedBellPair(..),
    TaggedBellPairs,
    (@), 
    TaggedRequiredRoots,
    History(..),
    DupKind(..),
    HasDupKinds(..),
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
    UTree(..),
    ) where

import           Data.Bifunctor             (bimap)
import           Data.Foldable              (toList)
import           Data.Functor.Contravariant (Predicate (..))
import           Data.Functor.Classes
import           Data.Monoid                (Endo (..))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.String                (IsString)
import           Data.Default
import qualified Data.Aeson as A
import qualified GHC.Exts                   (IsList, Item, fromList, toList)

import           Data.Vector.Fixed          (Arity, VecList)
import qualified Data.Vector.Fixed          as FV
import           Test.QuickCheck            hiding (choose, (.&&.))

import           BellKAT.Utils.Choice
import           BellKAT.Utils.Multiset     (Multiset)
import qualified BellKAT.Utils.Multiset     as Mset
import           BellKAT.Utils.UnorderedTree
--
-- * Type definitions

newtype Location = Location { name :: String } deriving newtype (Eq, Show, Ord, IsString)

-- | `:~:` is our symbol for entangled pair
newtype BellPair = BP (Location, Location) deriving newtype (Eq, Ord)

class LikeBellPair t where
    (~) :: Location -> Location -> t
    locations :: t -> (Location, Location)

instance LikeBellPair BellPair where
    l1 ~ l2 = if l1 <= l2 then BP (l1, l2) else BP (l2, l1)
    locations (BP x) = x

instance Show BellPair where
    show (BP (l1, l2)) = name l1 <> "~" <> name l2

hasLocation :: Location -> BellPair -> Bool
hasLocation l (BP (l1, l2)) = l == l1 || l == l2

type BellPairs = Multiset BellPair

-- | `BellPair` with an optional tag
data TaggedBellPair t = TaggedBellPair
    { bellPair    :: BellPair
    , bellPairTag :: t
    } deriving stock (Eq, Ord)

instance (Show t, Eq t, Default t) => Show (TaggedBellPair t) where
    showsPrec _ (TaggedBellPair bp t) 
        | t == def = shows bp
        | otherwise = shows bp . showString "/" . shows t

instance Default tag => LikeBellPair (TaggedBellPair tag) where
    l1 ~ l2 = TaggedBellPair (l1 ~ l2) def
    locations = locations . bellPair

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

type Probability = Rational

data CreateBellPairArgs tag = CreateBellPairArgs
    { cbpInputBPs    :: [TaggedBellPair tag] -- ^ a multiset of required (input) `BellPair`s
    , cbpOutputBP    :: TaggedBellPair tag -- ^ a produced (output) `BellPair`
    , cbpProbability :: Probability -- ^ probability of failure for operations that may fail
    , cbpDup         :: DupKind
    }

instance HasDupKinds (CreateBellPairArgs tag) where
  setDupKinds dk cbp = cbp { cbpDup = dk }
  modifyDupKinds f cbp = cbp { cbpDup = f (cbpDup cbp) }

instance Show1 CreateBellPairArgs where
  liftShowsPrec _ _ _ _ = shows "cbp"

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
    arbitrary = (~) <$> arbitrary <*> arbitrary

instance Arbitrary DupKind where
    arbitrary = DupKind <$> arbitrary <*> arbitrary

instance (Arbitrary t) => Arbitrary (TaggedBellPair t) where
    arbitrary = TaggedBellPair <$> arbitrary <*> arbitrary

instance A.ToJSON Location where
    toJSON = A.toJSON . name

instance A.FromJSON Location where
    parseJSON = fmap Location . A.parseJSON

instance Default t => A.ToJSON (TaggedBellPair t) where
    toJSON = A.toJSON . locations

instance Default t => A.FromJSON (TaggedBellPair t) where
    parseJSON v = uncurry (~) <$> A.parseJSON v
