{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData      #-}
module BellKAT.Implementations.TimelyHistoryQuantum (execute) where

import           Data.Foldable              (toList)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set

import           Data.Vector.Fixed          (pattern V2)

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Structures
import           BellKAT.Utils.Choice

-- ** Quantum operations represented as functions over histories recorded in a
-- _timely_ manner

type Time = Int

data TimelyHistoryQuantum t = TimelyHistoryQuantum
    { requiredRootsTimely :: [TaggedRequiredRoots t]
    , executeTimely       :: History t -> Set (History t, Time)
    }

instance Ord t => Semigroup (TimelyHistoryQuantum t) where
    -- | Definition of `<>` as sequential composition of `execute`
    hq <> hq' = TimelyHistoryQuantum
        { requiredRootsTimely = requiredRootsTimely hq
        , executeTimely = \h -> Set.fromList
            [(h'', t' + t'')
            | (h', t') <- Set.elems $ executeTimely hq h
            ,  (h'', t'') <- Set.elems $ executeTimely hq' h']
        }

instance Ord t => ParallelSemigroup (TimelyHistoryQuantum t) where
    --- | Definition of `<||>` as parallel composition
    hq <||> hq' = TimelyHistoryQuantum
        { requiredRootsTimely = requiredRootsTimely hq <> requiredRootsTimely hq'
        , executeTimely = \h ->
            Set.fromList
                [ (dupHistoryN (max t t') hRest
                    <> dupHistoryN (max t t' - t) hNew <> dupHistoryN (max t t' - t') hNew'
                  , max t t')
                | (V2 hs hs', hRest) <- toList $
                    chooseKHistories (V2 (requiredRootsTimely hq) (requiredRootsTimely hq')) h
                , (hNew, t) <- Set.elems $ executeTimely hq hs
                , (hNew', t') <- Set.elems $ executeTimely hq' hs'
                ]
        }

instance Ord t => CreatesBellPairs (TimelyHistoryQuantum t) t where
    tryCreateBellPairFrom (CreateBellPairArgs bps bp prob _dk) = TimelyHistoryQuantum
        { requiredRootsTimely = [bps]
        , executeTimely = \h@(History ts) ->
            case findTreeRootsND bps ts of
                [] -> [(dupHistory h, 1)]
                partialTsNews -> mconcat
                    [ case prob of
                        1.0 -> [(dupHistory (History tsRest) <> [Node bp tsNew], 1)]
                        _ ->  [(dupHistory (History tsRest) <> [Node bp tsNew], 1)
                                   ,(dupHistory (History tsRest), 1)]
                    | Partial { chosen = tsNew, rest = tsRest } <- partialTsNews
                    ]
        }

instance Ord t => Quantum (TimelyHistoryQuantum t) t where

execute :: Ord t => TimelyHistoryQuantum t -> History t -> Set (History t)
execute h = Set.map fst . executeTimely h
