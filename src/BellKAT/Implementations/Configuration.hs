module BellKAT.Implementations.Configuration
    ( NetworkCapacity (NC)
    , ExecutionParams(..)
    , fromNetworkCapacity
    , applyExecutionParams
    , fixNetworkCapacity
    ) where

import qualified GHC.Exts (IsList, Item)
import GHC.Exts (fromList, toList)
import Data.Default (Default(..))
import qualified Data.Map as Map
import qualified BellKAT.Utils.Multiset as Mset

import BellKAT.Definitions.Core
import BellKAT.Implementations.Output

-- | Network capacity, i.e., the maximum number of each possible `BellPair`, essentially
-- a `TaggedBellPairs`
newtype NetworkCapacity tag = NC
    { unNC :: TaggedBellPairs tag
    } deriving newtype (Monoid, Semigroup, Show)

instance Ord tag => GHC.Exts.IsList (NetworkCapacity tag) where
    type Item (NetworkCapacity tag) = TaggedBellPair tag
    fromList = NC . fromList
    toList = toList . unNC

-- | Combine a filter (cut-offs) with network capacity
-- rTag is the runtime tag for the Bell pairs, cTag is the clock/control tag
-- used alongside the runtime tag to filter pairs.
data ExecutionParams tag rTag cTag = EP
    { epNetworkCapacity :: Maybe (NetworkCapacity tag)
    , epFilter          :: TaggedBellPair rTag -> cTag -> Bool
    }
instance (Show tag, Eq tag, Default tag) => Show (ExecutionParams tag rTag cTag) where
    show (EP mbCap _) = "EP { epNetworkCapacity = " <> show mbCap <> ", epFilter = <function> }"

-- | Construct 'ExecutionParams' from an optional 'NetworkCapacity' and
-- a default permissive filter.
fromNetworkCapacity :: Maybe (NetworkCapacity tag) -> ExecutionParams tag rTag cTag
fromNetworkCapacity mbCap = EP { epNetworkCapacity = mbCap
                               , epFilter          = \_ _ -> True
                               }

instance Default (ExecutionParams tag rTag cTag) where
    def = EP { epNetworkCapacity = Nothing
             , epFilter          = \_ _ -> True
             }

-- | For each BellPair, keep at most the number allowed
fixNetworkCapacity
    :: (RuntimeTag rTag tag)
    => NetworkCapacity tag -> LabelledBellPairs cTag rTag -> LabelledBellPairs cTag rTag
fixNetworkCapacity (NC cap) (Mset.LMS (ms, t)) =
  let capCounts = countByBellPair (toList cap)
      grouped = Map.fromListWith (++) [(staticBellPair tbp, [tbp]) | tbp <- toList ms]
                                {- ^ duplicates with same key concatenate -}
      clipped = concat
        [ take (Map.findWithDefault maxBound bp capCounts) tbps
        {- ^ keeps only first n tagged instances for that BellPair -}
        | (bp, tbps) <- Map.toList grouped
        ]
  in Mset.LMS (Mset.fromList clipped, t)

-- | Count BellPairs in a TaggedBellPairs, disregarding tag
countByBellPair :: Tag tag => [TaggedBellPair tag] -> Map.Map (TaggedBellPair tag) Int
countByBellPair xs = Map.fromListWith (+) [ (staticBellPair tbp, 1) | tbp <- xs ]

-- | Apply filter and network capacity sequentially
applyExecutionParams
    :: (RuntimeTag rTag tag)
    => ExecutionParams tag rTag cTag
    -> LabelledBellPairs cTag rTag
    -> LabelledBellPairs cTag rTag
applyExecutionParams (EP mbCap bpPred) bps0 =
    let afterFilter = fixFilter bpPred bps0
    in maybe afterFilter (`fixNetworkCapacity` afterFilter) mbCap

-- | Filter labelled bell pairs using the given filter (predicate)
fixFilter :: (Ord rTag)
               => (TaggedBellPair rTag -> cTag -> Bool)
               -> LabelledBellPairs cTag rTag
               -> LabelledBellPairs cTag rTag
fixFilter bpFilter (Mset.LMS (ms, clock)) =
    let kept = filter (`bpFilter` clock) (GHC.Exts.toList ms)
    in Mset.LMS (Mset.fromList kept, clock)
