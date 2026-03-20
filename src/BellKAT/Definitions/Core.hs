{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE DeriveFunctor   #-}
module BellKAT.Definitions.Core (
    -- * Basic definitions
    Location(..),
    BellPair,
    LikeBellPair(..),
    BellPairs,
    Probability,
    Op(..),
    CreateBellPairArgs(..),
    CreateBellPairArgs',
    hasLocation,
    TaggedBellPair(..),
    TaggedBellPairs,
    LabelledBellPairs,
    (@), 
    TaggedRequiredRoots,
    History(..),
    -- * Dup for nodes in `History`
    dupHistory,
    dupForest,
    dupHistoryN,
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
import           Data.Aeson ((.:), (.=))
import           Control.Applicative ((<|>))
import qualified GHC.Exts                   (IsList, Item, fromList, toList)

import           Data.Vector.Fixed          (Arity, VecList)
import qualified Data.Vector.Fixed          as FV
import           Test.QuickCheck            hiding (choose, (.&&.))

import           BellKAT.Utils.Choice
import           BellKAT.Utils.Multiset     (Multiset, LabelledMultiset)
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
    } deriving stock (Eq, Ord, Functor)

instance (Show t, Eq t, Default t) => Show (TaggedBellPair t) where
    showsPrec _ (TaggedBellPair bp t) 
        | t == def = shows bp
        | otherwise = shows bp . shows t

instance Default tag => LikeBellPair (TaggedBellPair tag) where
    l1 ~ l2 = TaggedBellPair (l1 ~ l2) def
    locations = locations . bellPair

infix 8 @ -- less than of `(:~:)`

-- | Attaches tag to a Bell pair
(@) :: BellPair -> tag -> TaggedBellPair tag
(@) = TaggedBellPair

-- | `LabelledBellPairs` is a labelled multiset of Bell pairs, each with a tag
type LabelledBellPairs cTag tag = LabelledMultiset cTag (TaggedBellPair tag)

-- | `TaggedBellPairs` include tags for the BPs but not for the multiset
type TaggedBellPairs tag = LabelledMultiset () (TaggedBellPair tag)

type Probability = Rational
type StateQuality = Double
type CoherenceTime = Int 
type Distance = Int

-- ^ Describes the hardware details of a quantum operation irrespective of input and output
-- `BellPair`s, which are handled by `CreateBellPairArgs`
data Op tag =
    FSkip
    -- ^ yiels mempty
    | FCreate Probability StateQuality tag
    | FGenerate Probability StateQuality Distance tag
    | FTransmit Probability (CoherenceTime, CoherenceTime) Distance tag
    -- ^ all yield a probabilistic choice: singleton over the given TBP or empty
    -- | TODO: the tag here is redundant
    | FDestroy
    -- ^ yields mempty (destroyed Bell pair)
    | FSwap Probability (CoherenceTime, CoherenceTime, CoherenceTime) (Distance, Distance)
    -- ^ yields the swapped Bell pair given the two in input, with probability p
    | FDistill (CoherenceTime, CoherenceTime) Distance
    -- ^ yields the distilled Bell pair given the two same-location ones in input
    -- ^ computing the probability dynamically
    deriving stock (Eq, Ord, Show)

data CreateBellPairArgs op tag = CreateBellPairArgs
    { cbpInputBPs    :: [TaggedBellPair tag] -- ^ a list of required (input) `BellPair`s
    , cbpOutputBP    :: TaggedBellPair tag -- ^ a produced (output) `BellPair`
    , cbpOp          :: op -- ^ operation taking `cbpInputBPs` into `cbpOutputBP`, e.g., (`Op`)
    }

type CreateBellPairArgs' = CreateBellPairArgs Probability

instance Show1 (CreateBellPairArgs op) where
    liftShowsPrec _ _ _ _ = showString "cbp"

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
-- * Testing definitions

instance Arbitrary Location where
    arbitrary = Location <$> growingElements [[c] | c <- ['A'..'Z']]

instance Arbitrary BellPair where
    arbitrary = (~) <$> arbitrary <*> arbitrary

instance (Arbitrary t) => Arbitrary (TaggedBellPair t) where
    arbitrary = TaggedBellPair <$> arbitrary <*> arbitrary

instance A.ToJSON Location where
    toJSON = A.toJSON . name

instance A.FromJSON Location where
    parseJSON = fmap Location . A.parseJSON

instance A.ToJSON t => A.ToJSON (TaggedBellPair t) where
    toJSON (TaggedBellPair bp t) =
        A.object [ "locations" .= locations bp
                 , "tag"       .= t
                 ]

instance (A.FromJSON t, Default t) => A.FromJSON (TaggedBellPair t) where
    parseJSON v =
        -- format: { "locations": (l1,l2), "tag": t }
        A.withObject "TaggedBellPair" (\o -> do
            (l1, l2) <- o .: "locations"
            t        <- o .: "tag"
            pure $ TaggedBellPair (l1 ~ l2) t) v
        <|>
        -- format: (l1,l2)
        (do
            (l1, l2) <- A.parseJSON v
            pure $ TaggedBellPair (l1 ~ l2) def)
