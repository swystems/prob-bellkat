module BellKAT.Implementations.ProbAtomicOneStepQuantum
    ( ProbAtomicOneStepPolicy
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
import Control.Subcategory.Pointed ()
import Control.Subcategory.Functor
import Data.Typeable
import Data.Functor ()
import Data.Map ()

import qualified Data.Map                            as Map
import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Utils.Distribution as D
import BellKAT.Utils.Convex
import BellKAT.Definitions.Core
import BellKAT.Definitions.Tests
import BellKAT.Definitions.Structures
import BellKAT.Definitions.Atomic
import BellKAT.Implementations.QuantumOps
import BellKAT.Utils.Choice

-- | Essentially a symbol for `BellKAT.Utils.Automata.Guarded.GuardedFA` representing a set of
-- `ProbabilisticAtomicAction`s. In particular, it can be built from "basic actions" (`CreateBellPairArgs`). 
newtype ProbAtomicOneStepPolicy tag = ProbAtomicOneStepPolicy (Set (ProbabilisticAtomicAction tag)) 
    deriving newtype (Eq)
-- TODO: what's the difference between teh above design and the one in AtomicOneStepPolicy?

instance (Show tag, Ord tag, Default tag) => Show (ProbAtomicOneStepPolicy tag) where
    show (ProbAtomicOneStepPolicy xs) = "{" <> intercalate "," (show <$> Set.toList xs) <> "}"

instance Ord tag => GHC.Exts.IsList (ProbAtomicOneStepPolicy tag) where
    type Item (ProbAtomicOneStepPolicy tag) = ProbabilisticAtomicAction tag
    fromList = ProbAtomicOneStepPolicy . GHC.Exts.fromList
    toList (ProbAtomicOneStepPolicy xs) = GHC.Exts.toList xs

instance Ord tag => OrderedSemigroup (ProbAtomicOneStepPolicy tag) where
    x <.> y = fromList $ (<.>) <$> toList x <*> toList y

instance Ord tag => ParallelSemigroup (ProbAtomicOneStepPolicy tag) where
    x <||> y = fromList $ (<||>) <$> toList x <*> toList y

-- | Return the appropriate `Output` - the one yielding the correct distribution
instance Ord tag => CreatesBellPairs (ProbAtomicOneStepPolicy tag) tag where
    tryCreateBellPairFrom (CreateBellPairArgs i o p _) = ProbAtomicOneStepPolicy $ Set.fromList $
            [ createProbabilitsticAtomicAction
                (createRestrictedTest mempty)
                (Mset.fromList $ map (`TaggedBellPair` ()) i)
                -- Pattern matching --
                (
                    -- create, transmit, generate
                    -- without distinction, output a probability distributions over default TBP
                    -- EXTENSION: in the full model we will have to differentiate (no default ^) 
                    -- as cr, tr, gen, in general, yield different time and fidelity
                    if length i <= 1
                    then
                        (if p == 1 then Do (getDefaultQuantumBellPair o)
                        else if p == 0 then NoOp
                        else Try p (getDefaultQuantumBellPair o))
                    
                    -- distill, swap
                    -- a distill does not have a specified probability (marked it using -1)
                    -- whereas a swap has a specified probability
                    else if length i == 2
                    then
                        (if p == 0 then NoOp
                        else if p == (-1) then Distill
                        else Swap p)
                    -- no action should have >2 required Bell pairs in this model
                    else error "ProbAtomicOneStepPolicy: not a valid action"
                )
            ] <> 
                if i /= mempty 
                then [createProbabilitsticAtomicAction 
                        (createRestrictedTest  [Mset.fromList $ map (`TaggedBellPair` ()) i])
                        mempty 
                        NoOp]
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
execute :: (Typeable tag, Show tag, Default tag, Ord tag, ValidTag tag) => ProbAtomicOneStepPolicy tag -> TaggedBellPairs tag -> CD' (TaggedBellPairs tag)
execute (ProbAtomicOneStepPolicy xs) bps = 
    foldMap (\paa -> executePAA id paa bps) xs

execute' :: (Typeable tag, RationalOrDouble p, Show tag, Default tag, Ord tag, ValidTag tag) => ProbAtomicOneStepPolicy tag -> TaggedBellPairs tag -> CD p (TaggedBellPairs tag)
execute' p bps = mapProbability fromRational $ execute p bps

executeWithCapacity :: (Typeable tag, Show tag, Default tag, Ord tag, ValidTag tag) => NetworkCapacity tag -> ProbAtomicOneStepPolicy tag -> TaggedBellPairs tag -> CD' (TaggedBellPairs tag)
executeWithCapacity nc (ProbAtomicOneStepPolicy xs) bps = 
    foldMap (\paa -> executePAA (fixNetworkCapacity nc) paa bps) xs

executeWithCapacity' :: (Typeable tag, RationalOrDouble p, Show tag, Default tag, Ord tag, ValidTag tag) => NetworkCapacity tag -> ProbAtomicOneStepPolicy tag -> TaggedBellPairs tag -> CD p (TaggedBellPairs tag)
executeWithCapacity' nc p bps = mapProbability fromRational $ executeWithCapacity nc p bps

executePAA :: (Show tag, Ord tag, Default tag, Typeable tag, ValidTag tag)
           => (TaggedBellPairs tag -> TaggedBellPairs tag)
           -- ^ "fixing" function to apply at the end
           -> ProbabilisticAtomicAction tag
           -> TaggedBellPairs tag
           -> CD' (TaggedBellPairs tag)
executePAA fix act bps = 
    if (getBPsPredicate . toBPsPredicate . paaTest) act (untagBellPairs' bps)
                                                            {- ^ location-only test -} 
       then fromList
        [ cmap (fix . (<> rest partial) )
                    {- ^ adds the untouched BPs -} 
                      (asFunction (paaOutputBPD act) (chosen partial) )
                      {- ^ applies the output function to the chosen pairs -}
        | let requiredBPs = map untagBellPair (toList (paaInputBPs act))
                             {- ^ applies the output function to the chosen pairs -}
        , partial <- findElemsND untagBellPair requiredBPs bps
        ]
       else mempty

-- | TODO: maybe move the functions below to utils?

-- | Remove tags from a TaggedBellPair, yielding the underlying BellPair
untagBellPair :: (Ord tag) => TaggedBellPair tag -> BellPair
untagBellPair (TaggedBellPair bp _) = bp

-- | Remove tags from a multiset of tagged Bell pairs, yielding stripped TaggedBellPairs
untagBellPairs' :: (Ord tag) => TaggedBellPairs tag -> TaggedBellPairs ()
untagBellPairs' = Mset.map (\(TaggedBellPair bp _) -> TaggedBellPair bp ())

-- | Count BellPairs in a TaggedBellPairs, disregarding tag
countByBellPair :: Ord tag => [TaggedBellPair tag] -> Map.Map BellPair Int
countByBellPair xs = Map.fromListWith (+) [ (untagBellPair tbp, 1) | tbp <- xs ]

-- | For each BellPair, keep at most the number allowed
fixNetworkCapacity :: Ord tag => NetworkCapacity tag -> TaggedBellPairs tag -> TaggedBellPairs tag
fixNetworkCapacity (NC cap) xs =
  let capCounts = countByBellPair (toList cap)
      grouped = Map.fromListWith (++) [(untagBellPair tbp, [tbp]) | tbp <- toList xs]
                                {- ^ duplicates with same key concatenate -}
      clipped = concat
        [ take (Map.findWithDefault maxBound bp capCounts) tbps
        {- ^ keeps only first n tagged instances for that BellPair -}
        | (bp, tbps) <- Map.toList grouped
        ]
  in fromList clipped