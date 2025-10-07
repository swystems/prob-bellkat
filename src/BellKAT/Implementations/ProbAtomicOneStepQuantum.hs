module BellKAT.Implementations.ProbAtomicOneStepQuantum
    ( ProbAtomicOneStepPolicy
    , ProbAtomicOneStepPolicy'
    , NetworkCapacity (NC)
    , execute
    , execute'
    , executeWithCapacity
    , executeWithCapacity'
    ) where

import qualified GHC.Exts (IsList, Item)
import GHC.Exts (fromList, toList)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default
import Control.Subcategory.Functor

import qualified Data.Map                            as Map
import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Utils.Distribution (D', RationalOrDouble, DDom)
import qualified BellKAT.Utils.Distribution as D
import BellKAT.Utils.Convex
import BellKAT.Definitions.Core
import BellKAT.Definitions.Tests
import BellKAT.Definitions.Structures
import BellKAT.Definitions.Atomic
import BellKAT.Utils.Choice
import BellKAT.Implementations.Output

-- | Essentially a symbol for `BellKAT.Utils.Automata.Guarded.GuardedFA` representing a set of
-- `ProbabilisticAtomicAction`s. In particular, it can be built from "basic actions" (`CreateBellPairArgs`). 
newtype ProbAtomicOneStepPolicy output tag = ProbAtomicOneStepPolicy (Set (ProbabilisticAtomicAction output tag))
    deriving newtype (Eq)
-- TODO: what's the difference between teh above design and the one in AtomicOneStepPolicy?

type ProbAtomicOneStepPolicy' tag = ProbAtomicOneStepPolicy (D' (TaggedBellPairs tag)) tag

instance (Show output, Show tag, Ord tag, Default tag) => Show (ProbAtomicOneStepPolicy output tag) where
    show (ProbAtomicOneStepPolicy xs) = "{" <> intercalate "," (show <$> Set.toList xs) <> "}"

instance (Ord output, Ord tag) => GHC.Exts.IsList (ProbAtomicOneStepPolicy output tag) where
    type Item (ProbAtomicOneStepPolicy output tag) = ProbabilisticAtomicAction output tag
    fromList = ProbAtomicOneStepPolicy . GHC.Exts.fromList
    toList (ProbAtomicOneStepPolicy xs) = GHC.Exts.toList xs

instance (Monoid output, Ord output, Ord tag) => OrderedSemigroup (ProbAtomicOneStepPolicy output tag) where
    x <.> y = fromList $ (<.>) <$> toList x <*> toList y

instance (Monoid output, Ord output, Ord tag) => ParallelSemigroup (ProbAtomicOneStepPolicy output tag) where
    x <||> y = fromList $ (<||>) <$> toList x <*> toList y

instance (OpOutput output op tag, Monoid output, Ord output, Ord tag)
        => CreatesBellPairs (ProbAtomicOneStepPolicy output tag) op tag where
    tryCreateBellPairFrom (CreateBellPairArgs i o p _) = ProbAtomicOneStepPolicy $ Set.fromList $
            [ createProbabilitsticAtomicAction
                (createRestrictedTest mempty)
                (Mset.fromList i)
                (fromCBPOutput (Mset.fromList i) o p)
            ] <>
                if i /= mempty
                then [createProbabilitsticAtomicAction
                        (createRestrictedTest  [Mset.fromList i])
                        mempty
                        mempty]
                else mempty

-- | Network capacity, i.e., the maximum number of each possible `BellPair`, essentially
-- a `TaggedBellPairs`
newtype NetworkCapacity tag = NC 
    { unNC :: TaggedBellPairs tag 
    } deriving newtype (Monoid, Semigroup)
    -- TODO: should really be BellPairs

instance Ord tag => GHC.Exts.IsList (NetworkCapacity tag) where
    type Item (NetworkCapacity tag) = TaggedBellPair tag
    fromList = NC . fromList
    toList = toList . unNC

-- Interprets `ProbAtomicOneStepPolicy` as a monadic function from `TaggedBellPairs` to `CD'` of
-- `TaggedBellPairs`
execute
    :: (Output output tag, Ord tag)
    => (DDom (RTag output), Default (RTag output))
    => ProbAtomicOneStepPolicy output tag 
    -> TaggedBellPairs (RTag output) -> CD' (TaggedBellPairs (RTag output))
execute (ProbAtomicOneStepPolicy xs) bps =
    foldMap (\paa -> executePAA id paa bps) xs

execute'
    :: (Output output tag, RationalOrDouble p, Ord tag)
    => (DDom (RTag output), Default (RTag output))
    => ProbAtomicOneStepPolicy output tag 
    -> TaggedBellPairs (RTag output) -> CD p (TaggedBellPairs (RTag output))
execute' p bps = D.mapProbability fromRational $ execute p bps

executeWithCapacity
    :: (Output output tag, Ord tag)
    => (DDom (RTag output), Default (RTag output))
    => NetworkCapacity tag
    -> ProbAtomicOneStepPolicy output tag
    -> TaggedBellPairs (RTag output) -> CD' (TaggedBellPairs (RTag output))
executeWithCapacity nc (ProbAtomicOneStepPolicy xs) bps =
    foldMap (\paa -> executePAA (fixNetworkCapacity nc) paa bps) xs

executeWithCapacity'
    :: (Output output tag, RuntimeTag (RTag output) tag, RationalOrDouble p, Ord tag)
    => (DDom (RTag output), Default (RTag output))
    => NetworkCapacity tag -> ProbAtomicOneStepPolicy output tag
    -> TaggedBellPairs (RTag output) -> CD p (TaggedBellPairs (RTag output))
executeWithCapacity' nc p bps = D.mapProbability fromRational $ executeWithCapacity nc p bps

executePAA :: (Output output tag, RuntimeTag (RTag output) tag) 
           => (Ord tag)
           => (DDom (RTag output), Default (RTag output))
           => (TaggedBellPairs (RTag output) -> TaggedBellPairs (RTag output))
           -- ^ "fixing" function to apply at the end
           -> ProbabilisticAtomicAction output tag 
           -> TaggedBellPairs (RTag output) -> CD' (TaggedBellPairs (RTag output))
executePAA fix act bps =
    let testHolds = (getBPsPredicate . toBPsPredicate . paaTest) act (staticBellPairs bps)
     in if testHolds
        then mconcat
         [ cmap (fix . (<> rest)) (computeOutput (paaOutput act) chosen)
           | Partial { chosen , rest }  <- findElemsND' (fmap staticTag) (toList . paaInputBPs $ act) bps]
        else mempty

-- executePAA _ act@(ProbabilisticAtomicAction _ []) untouched =
--         then fromList [cpure untouched]
-- executePAA fix act@(ProbabilisticAtomicAction _ ((i,o):ios)) bps =
--         then fromList
--                 [ cmap fix $ cmap (uncurry (<>)) $ pair (asFunction o (chosen partial)) dTail
--                 | let requiredBPs = map untagBellPair (toList i)
--                 , partial <- findElemsND untagBellPair requiredBPs bps
--                 , let tailDist = executePAA fix (ProbabilisticAtomicAction (paaTest act) ios) (rest partial)
--                 , dTail <- toList tailDist
--                 ]

-- | For each BellPair, keep at most the number allowed
fixNetworkCapacity 
    :: (RuntimeTag rTag tag, Ord rTag, Ord tag)
    => NetworkCapacity tag -> TaggedBellPairs rTag -> TaggedBellPairs rTag
fixNetworkCapacity (NC cap) xs =
  let capCounts = countByBellPair (toList cap)
      grouped = Map.fromListWith (++) [(staticBellPair tbp, [tbp]) | tbp <- toList xs]
                                {- ^ duplicates with same key concatenate -}
      clipped = concat
        [ take (Map.findWithDefault maxBound bp capCounts) tbps
        {- ^ keeps only first n tagged instances for that BellPair -}
        | (bp, tbps) <- Map.toList grouped
        ]
  in fromList clipped

-- | Count BellPairs in a TaggedBellPairs, disregarding tag
countByBellPair :: Ord tag => [TaggedBellPair tag] -> Map.Map (TaggedBellPair tag) Int
countByBellPair xs = Map.fromListWith (+) [ (staticBellPair tbp, 1) | tbp <- xs ]


-- TODO: "old" version
-- fixNetworkCapacity 
--     :: (RuntimeTag rTag tag, Ord rTag, Ord tag) 
--     => NetworkCapacity tag -> TaggedBellPairs rTag -> TaggedBellPairs rTag
-- fixNetworkCapacity (NC nc) = fst . foldl' 
--     (\(acc, nc') bp -> if (staticTag <$> bp) `Mset.member` nc' 
--                          then (acc <> Mset.singleton bp, Mset.remove (staticTag <$> bp) nc') 
--                          else (acc, nc'))
--     (mempty, nc)

