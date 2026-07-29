{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Implementations.MDPExtremal
    ( ConcreteMDPState
    , ExtremalQuery(..)
    , CoverageStatus(..)
    , SchedulerChoiceTrace(..)
    , SchedulerChoice(..)
    , SchedulerSelection(..)
    , ExtremalResult(..)
    , ExtremalSeriesResult(..)
    , ExtremalSummary(..)
    , computeExtremalReachability
    , computeExtremalReachabilityRolling
    , computeExtremalReachabilitySummary
    , extremalDPTablesToJSON
    , renderExtremalResult
    , renderExtremalSeriesResult
    , renderExtremalSummary
    , renderExtremalDPTables
    ) where

import qualified Data.Aeson                  as A
import           Data.Graph                  (SCC (..), stronglyConnComp)
import           Data.List                   (foldl', intercalate, mapAccumL, transpose, zipWith5)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 (Sum (..))
import qualified Data.IntMap.Strict          as IM
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import           GHC.Exts                    (IsList, Item, toList)
import           Numeric                     (showFFloat)

import           BellKAT.Utils.MDP
    ( MDP(..)
    , StepCost(..)
    )
import           BellKAT.Utils.Automata.Transitions.Functorial (StateSystem(..))
import           BellKAT.Utils.Convex        (getGenerators)
import           BellKAT.Utils.Distribution  (D, RationalOrDouble, toDouble)
import qualified BellKAT.Utils.Distribution  as D

type ConcreteMDPState s = (Int, s)

type ExtremalTable s p = Map.Map (ConcreteMDPState s) (IM.IntMap p)

type Action s p = D p (ConcreteMDPState s, StepCost)

type SchedulerChoiceLog s p = Map.Map (ConcreteMDPState s) [SchedulerChoice s p]

-- | Compact scheduler trace for one state.
--
-- The trace only stores change points: each 'SchedulerChoice' applies from its
-- budget until the next listed change point for the same state.
data SchedulerChoiceTrace s p = SchedulerChoiceTrace
    { sctState :: ConcreteMDPState s
    , sctChanges :: [SchedulerChoice s p]
    }
    deriving stock (Eq, Show)

-- | Scheduler choice beginning at a particular budget.
data SchedulerChoice s p = SchedulerChoice
    { scBudget :: Int
    , scSelection :: SchedulerSelection s p
    }
    deriving stock (Eq, Show)

data SchedulerSelection s p
    = ChosenAction Int p [(Int, p)] (Action s p)
    | AllActionsSameValue p [(Int, p)]
    deriving stock (Eq, Show)

data BudgetCell s p = BudgetCell
    { bcValue :: p
    , bcChoice :: Maybe (ConcreteMDPState s, SchedulerChoice s p)
    }

data ExtremalQuery
    = ExtremalBudget Int
    | ExtremalCoverage Double
    deriving stock (Eq, Show)

data CoverageStatus p
    = CoverageReached
        { coverageTarget :: Double
        , coverageBudget :: Int
        , coverageValue :: p
        }
    | CoverageUnreachable
        { coverageTarget :: Double
        , coverageBudget :: Int
        , coverageValue :: p
        }
    deriving stock (Eq, Show)

data ExtremalResult s p = ExtremalResult
    { erInitialState :: ConcreteMDPState s
    , erStates :: [ConcreteMDPState s]
    , erGoalStates :: [ConcreteMDPState s]
    , erResolvedBudget :: Int
    , erMinTable :: ExtremalTable s p
    , erMaxTable :: ExtremalTable s p
    , erMinSchedulerChoices :: [SchedulerChoiceTrace s p]
    , erMaxSchedulerChoices :: [SchedulerChoiceTrace s p]
    , erCoverageStatus :: Maybe (CoverageStatus p)
    }
    deriving stock (Eq, Show)

-- | CDF-compatible result produced with rolling per-state DP storage.
--
-- This deliberately exposes series rather than the partial internal tables,
-- preventing callers from treating evicted cells as zero-valued DP entries.
data ExtremalSeriesResult s p = ExtremalSeriesResult
    { esrInitialState :: ConcreteMDPState s
    , esrStates :: [ConcreteMDPState s]
    , esrGoalStates :: [ConcreteMDPState s]
    , esrResolvedBudget :: Int
    , esrCDFMin :: [p]
    , esrCDFMax :: [p]
    , esrMinSchedulerChoices :: [SchedulerChoiceTrace s p]
    , esrMaxSchedulerChoices :: [SchedulerChoiceTrace s p]
    , esrCoverageStatus :: Maybe (CoverageStatus p)
    }
    deriving stock (Eq, Show)

-- | Endpoint-only result for the rolling-window solver.
--
-- Unlike 'ExtremalResult', this type deliberately does not retain the CDF
-- prefix, scheduler trace, or full per-state DP tables.  It can therefore be
-- produced with O((c_max + 1) * N) DP storage.
data ExtremalSummary s p = ExtremalSummary
    { esInitialState :: !(ConcreteMDPState s)
    , esStateCount :: !Int
    , esGoalStateCount :: !Int
    , esResolvedBudget :: !Int
    , esCDFMin :: !p
    , esCDFMax :: !p
    , esCoverageStatus :: !(Maybe (CoverageStatus p))
    , esMaxPositiveCost :: !Int
    , esRetainedColumns :: !Int
    }
    deriving stock (Eq, Show)

data TableRetention
    = RetainFullTable
    | RetainInitialStatePrefix
    | RetainRollingWindow

data ChoiceRetention
    = RetainChoiceTrace
    | DiscardChoiceTrace

instance RationalOrDouble p => A.ToJSON (CoverageStatus p) where
    toJSON status =
        case status of
            CoverageReached target budget value ->
                coverageToJSON "reached" target budget value
            CoverageUnreachable target budget value ->
                coverageToJSON "unreachable" target budget value
      where
        coverageToJSON :: RationalOrDouble p => String -> Double -> Int -> p -> A.Value
        coverageToJSON kind target budget value =
            A.object
                [ "status" A..= kind
                , "target" A..= target
                , "budget" A..= budget
                , "value" A..= toDouble value
                ]

instance (Ord s, Show s, IsList s, Show (Item s), Show p, RationalOrDouble p) => A.ToJSON (ExtremalResult s p) where
    toJSON result =
        let (cdfMin, cdfMax) = initialStateCDFSeries result
         in A.object
                [ "initial_state" A..= stateToJSON (erInitialState result)
                , "states" A..= fmap stateToJSON (erStates result)
                , "goal_states" A..= fmap stateToJSON (erGoalStates result)
                , "resolved_budget" A..= erResolvedBudget result
                , "coverage_status" A..= erCoverageStatus result
                , "series" A..=
                    A.object
                        [ "cdf_min" A..= fmap toDouble cdfMin
                        , "cdf_max" A..= fmap toDouble cdfMax
                        ]
                , "scheduler_choices" A..=
                    A.object
                        [ "min" A..= fmap schedulerChoiceTraceToJSON (erMinSchedulerChoices result)
                        , "max" A..= fmap schedulerChoiceTraceToJSON (erMaxSchedulerChoices result)
                        ]
                ]

instance (Show s, IsList s, Show (Item s), Show p, RationalOrDouble p) => A.ToJSON (ExtremalSeriesResult s p) where
    toJSON result =
        A.object
            [ "initial_state" A..= stateToJSON (esrInitialState result)
            , "states" A..= fmap stateToJSON (esrStates result)
            , "goal_states" A..= fmap stateToJSON (esrGoalStates result)
            , "resolved_budget" A..= esrResolvedBudget result
            , "coverage_status" A..= esrCoverageStatus result
            , "series" A..=
                A.object
                    [ "cdf_min" A..= fmap toDouble (esrCDFMin result)
                    , "cdf_max" A..= fmap toDouble (esrCDFMax result)
                    ]
            , "scheduler_choices" A..=
                A.object
                    [ "min" A..= fmap schedulerChoiceTraceToJSON (esrMinSchedulerChoices result)
                    , "max" A..= fmap schedulerChoiceTraceToJSON (esrMaxSchedulerChoices result)
                    ]
            ]

instance (Show s, IsList s, Show (Item s), RationalOrDouble p) => A.ToJSON (ExtremalSummary s p) where
    toJSON summary =
        A.object
            [ "result_kind" A..= ("rolling_summary" :: String)
            , "schema_version" A..= (1 :: Int)
            , "initial_state" A..= stateToJSON (esInitialState summary)
            , "state_count" A..= esStateCount summary
            , "goal_state_count" A..= esGoalStateCount summary
            , "resolved_budget" A..= esResolvedBudget summary
            , "coverage_status" A..= esCoverageStatus summary
            , "cdf_min" A..= toDouble (esCDFMin summary)
            , "cdf_max" A..= toDouble (esCDFMax summary)
            , "max_positive_cost" A..= esMaxPositiveCost summary
            , "retained_columns" A..= esRetainedColumns summary
            ]

extremalDPTablesToJSON
    :: (Ord s, Show s, IsList s, Show (Item s), RationalOrDouble p)
    => ExtremalResult s p
    -> A.Value
extremalDPTablesToJSON result =
    A.object
        [ "columns" A..= initialStateTimeSeries result
        , "min" A..= tableRowsToJSON (erMinTable result)
        , "max" A..= tableRowsToJSON (erMaxTable result)
        ]
  where
    tableRowsToJSON table =
        [ A.object
            [ "state" A..= stateToJSON st
            , "values" A..= fmap toDouble (cdfRow table st (erResolvedBudget result))
            ]
        | st <- erStates result
        ]

stateToJSON :: (Show s, IsList s, Show (Item s)) => ConcreteMDPState s -> A.Value
stateToJSON st@(pc, bps) =
    A.object
        [ "pc" A..= pc
        , "bell_pairs" A..= fmap show (toList bps)
        , "rendered" A..= show st
        ]

schedulerChoiceTraceToJSON
    :: (Show s, IsList s, Show (Item s), Show p, RationalOrDouble p)
    => SchedulerChoiceTrace s p
    -> A.Value
schedulerChoiceTraceToJSON trace =
    A.object
        [ "state" A..= stateToJSON (sctState trace)
        , "changes" A..= fmap schedulerChoiceToJSON (sctChanges trace)
        ]

schedulerChoiceToJSON
    :: (Show s, Show p, RationalOrDouble p)
    => SchedulerChoice s p
    -> A.Value
schedulerChoiceToJSON choice =
    A.object $
        [ "budget" A..= scBudget choice ]
        <> case scSelection choice of
            ChosenAction actionIndex value actionValues action ->
                [ "kind" A..= ("chosen_action" :: String)
                , "action_index" A..= actionIndex
                , "value" A..= toDouble value
                , "action_values" A..= fmap actionValueToJSON actionValues
                , "action" A..= renderAction action
                ]
            AllActionsSameValue value actionValues ->
                [ "kind" A..= ("all_actions_same_value" :: String)
                , "value" A..= toDouble value
                , "action_values" A..= fmap actionValueToJSON actionValues
                ]
  where
    actionValueToJSON (actionIndex, value) =
        A.object
            [ "action_index" A..= actionIndex
            , "value" A..= toDouble value
            ]

computeExtremalReachability
    :: (Ord s, Show s, RationalOrDouble p)
    => (s -> Bool)
    -> ExtremalQuery
    -> StateSystem (MDP p) s
    -> Either String (ExtremalResult s p)
computeExtremalReachability =
    computeExtremalReachabilityWith RetainFullTable

-- | Compute the ordinary CDF series while retaining only the rolling DP window
-- for non-initial states.  The distinct result type exposes the complete
-- initial-state series without exposing the evicted internal table cells.
computeExtremalReachabilityRolling
    :: (Ord s, Show s, RationalOrDouble p)
    => (s -> Bool)
    -> ExtremalQuery
    -> StateSystem (MDP p) s
    -> Either String (ExtremalSeriesResult s p)
computeExtremalReachabilityRolling isGoal query ss = do
    result <- computeExtremalReachabilityWith RetainInitialStatePrefix isGoal query ss
    let (cdfMin, cdfMax) = initialStateCDFSeries result
    pure $
        ExtremalSeriesResult
            { esrInitialState = erInitialState result
            , esrStates = erStates result
            , esrGoalStates = erGoalStates result
            , esrResolvedBudget = erResolvedBudget result
            , esrCDFMin = cdfMin
            , esrCDFMax = cdfMax
            , esrMinSchedulerChoices = erMinSchedulerChoices result
            , esrMaxSchedulerChoices = erMaxSchedulerChoices result
            , esrCoverageStatus = erCoverageStatus result
            }

computeExtremalReachabilityWith
    :: (Ord s, Show s, RationalOrDouble p)
    => TableRetention
    -> (s -> Bool)
    -> ExtremalQuery
    -> StateSystem (MDP p) s
    -> Either String (ExtremalResult s p)
computeExtremalReachabilityWith tableRetention isGoal query ss = do
    validateExtremalQuery query
    let states = collectConcreteStates ss
        goalStates = filter (isGoal . snd) states
        goalSet = Set.fromList goalStates
        actions = buildActionMap ss states

    validateNonNegativeCosts goalSet actions
    validateZeroCostAcyclic goalSet actions

    let (minTable, resolvedBudget, coverageStatus, minChoices) =
            computeExtremalTable
                tableRetention
                RetainChoiceTrace
                selectMinAction
                query
                states
                goalSet
                actions
                (ssInitial ss)
        deterministic = all ((<= 1) . length) (Map.elems actions)
        (maxTable, maxChoices) =
            if deterministic
               then (minTable, minChoices)
               else
                    let (table, _, _, choices) =
                            computeExtremalTable
                                tableRetention
                                RetainChoiceTrace
                                selectMaxAction
                                (ExtremalBudget resolvedBudget)
                                states
                                goalSet
                                actions
                                (ssInitial ss)
                     in (table, choices)

    pure $
        ExtremalResult
            { erInitialState = ssInitial ss
            , erStates = states
            , erGoalStates = goalStates
            , erResolvedBudget = resolvedBudget
            , erMinTable = minTable
            , erMaxTable = maxTable
            , erMinSchedulerChoices = minChoices
            , erMaxSchedulerChoices = maxChoices
            , erCoverageStatus = coverageStatus
            }

-- | Compute only the two endpoint probabilities.  This omits CDF prefixes and
-- scheduler traces so every state row can use the bounded rolling window.
computeExtremalReachabilitySummary
    :: (Ord s, Show s, RationalOrDouble p)
    => (s -> Bool)
    -> ExtremalQuery
    -> StateSystem (MDP p) s
    -> Either String (ExtremalSummary s p)
computeExtremalReachabilitySummary isGoal query ss = do
    validateExtremalQuery query
    let states = collectConcreteStates ss
        goalStates = filter (isGoal . snd) states
        goalSet = Set.fromList goalStates
        actions = buildActionMap ss states
        initialState = ssInitial ss
        maxPositiveCost = largestPositiveCost goalSet actions

    validateNonNegativeCosts goalSet actions
    validateZeroCostAcyclic goalSet actions

    let (minTable, resolvedBudget, coverageStatus, _) =
            computeExtremalTable
                RetainRollingWindow
                DiscardChoiceTrace
                selectMinAction
                query
                states
                goalSet
                actions
                initialState
        deterministic = all ((<= 1) . length) (Map.elems actions)
        maxTable =
            if deterministic
               then minTable
               else
                    let (table, _, _, _) =
                            computeExtremalTable
                                RetainRollingWindow
                                DiscardChoiceTrace
                                selectMaxAction
                                (ExtremalBudget resolvedBudget)
                                states
                                goalSet
                                actions
                                initialState
                     in table

    pure $
        ExtremalSummary
            { esInitialState = initialState
            , esStateCount = length states
            , esGoalStateCount = length goalStates
            , esResolvedBudget = resolvedBudget
            , esCDFMin = tableValue minTable initialState resolvedBudget
            , esCDFMax = tableValue maxTable initialState resolvedBudget
            , esCoverageStatus = coverageStatus
            , esMaxPositiveCost = maxPositiveCost
            , esRetainedColumns = maximum (0 : fmap IM.size (Map.elems minTable))
            }

renderExtremalResult :: (Ord s, RationalOrDouble p, Show s) => ExtremalResult s p -> String
renderExtremalResult result =
    unlines $
        [ "Extremal cost-bounded reachability"
        , "Initial state: " <> show (erInitialState result)
        , "Goal states: " <> renderStateList (erGoalStates result)
        , "Computed up to budget: " <> show (erResolvedBudget result)
        ]
        <> maybe [] (\status -> [renderCoverageStatus status]) (erCoverageStatus result)
        <> [ ""
           , renderTable
                ["t", "pmf_min[t]", "pmf_max[t]", "cdf_min[t]", "cdf_max[t]"]
                [ [ show t
                  , show pmfMin
                  , show pmfMax
                  , show cdfMin
                  , show cdfMax
                  ]
                | (t, pmfMin, pmfMax, cdfMin, cdfMax) <- initialStateRows result
                ]
           , ""
           , renderSchedulerChoices "Worst scheduler choices (min CDF):" (erMinSchedulerChoices result)
           , renderSchedulerChoices "Best scheduler choices (max CDF):" (erMaxSchedulerChoices result)
           ]

renderExtremalSeriesResult
    :: (RationalOrDouble p, Show s)
    => ExtremalSeriesResult s p
    -> String
renderExtremalSeriesResult result =
    unlines $
        [ "Extremal cost-bounded reachability"
        , "Initial state: " <> show (esrInitialState result)
        , "Goal states: " <> renderStateList (esrGoalStates result)
        , "Computed up to budget: " <> show (esrResolvedBudget result)
        ]
        <> maybe [] (pure . renderCoverageStatus) (esrCoverageStatus result)
        <> [ ""
           , renderTable
                ["t", "pmf_min[t]", "pmf_max[t]", "cdf_min[t]", "cdf_max[t]"]
                [ [ show t
                  , show pmfMin
                  , show pmfMax
                  , show cdfMin
                  , show cdfMax
                  ]
                | (t, pmfMin, pmfMax, cdfMin, cdfMax) <- seriesResultRows result
                ]
           , ""
           , renderSchedulerChoices
                "Worst scheduler choices (min CDF):"
                (esrMinSchedulerChoices result)
           , renderSchedulerChoices
                "Best scheduler choices (max CDF):"
                (esrMaxSchedulerChoices result)
           ]

renderExtremalSummary :: (Show s, RationalOrDouble p) => ExtremalSummary s p -> String
renderExtremalSummary summary =
    unlines $
        [ "Extremal cost-bounded reachability (rolling summary)"
        , "Initial state: " <> show (esInitialState summary)
        , "States: " <> show (esStateCount summary)
        , "Goal states: " <> show (esGoalStateCount summary)
        , "Computed up to budget: " <> show (esResolvedBudget summary)
        ]
        <> maybe [] (pure . renderCoverageStatus) (esCoverageStatus summary)
        <> [ "CDF minimum: " <> show (esCDFMin summary)
           , "CDF maximum: " <> show (esCDFMax summary)
           , "Largest positive transition cost: " <> show (esMaxPositiveCost summary)
           , "Retained DP columns: " <> show (esRetainedColumns summary)
           ]

renderExtremalDPTables :: (Ord s, RationalOrDouble p, Show s) => ExtremalResult s p -> String
renderExtremalDPTables result =
    unlines
        [ "DP table dump"
        , ""
        , renderDPTable "Min DP table:" (erMinTable result)
        , ""
        , renderDPTable "Max DP table:" (erMaxTable result)
        ]
  where
    renderDPTable title table =
        unlines $
            title :
            lines
                ( renderTable
                    ("state" : fmap (("t=" <>) . show) (initialStateTimeSeries result))
                    [ show st : fmap show (cdfRow table st (erResolvedBudget result))
                    | st <- erStates result
                    ]
                )

renderStateList :: Show s => [ConcreteMDPState s] -> String
renderStateList [] = "none"
renderStateList xs = intercalate ", " (show <$> xs)

renderCoverageStatus :: Show p => CoverageStatus p -> String
renderCoverageStatus (CoverageReached target budget value) =
    "Coverage target " <> show target
        <> " reached for the worst scheduler at budget "
        <> show budget <> " with cdf_min[t] = " <> show value
renderCoverageStatus (CoverageUnreachable target budget value) =
    "Coverage target " <> show target
        <> " was not reached; the worst-scheduler CDF stabilised by budget "
        <> show budget <> " at cdf_min[t] = " <> show value

renderSchedulerChoices :: (Show s, RationalOrDouble p) => String -> [SchedulerChoiceTrace s p] -> String
renderSchedulerChoices title [] =
    unlines [title, "  none"]
renderSchedulerChoices title traces =
    unlines $ title : concatMap renderSchedulerChoiceTrace traces

renderSchedulerChoiceTrace :: (Show s, RationalOrDouble p) => SchedulerChoiceTrace s p -> [String]
renderSchedulerChoiceTrace trace =
    ("  state=" <> show (sctState trace))
        : fmap (("    " <>) . renderSchedulerChoice) (sctChanges trace)

renderSchedulerChoice :: (Show s, RationalOrDouble p) => SchedulerChoice s p -> String
renderSchedulerChoice choice =
    "from t=" <> show (scBudget choice)
        <> case scSelection choice of
            ChosenAction actionIndex value actionValues action ->
                ": choose action #" <> show actionIndex
                    <> " with value " <> formatSchedulerValue value
                    <> " as (" <> renderActionValues actionValues <> ")"
                    <> " -> " <> renderAction action
            AllActionsSameValue value actionValues ->
                ": all actions have same value " <> formatSchedulerValue value
                    <> " as (" <> renderActionValues actionValues <> ")"

renderActionValues :: RationalOrDouble p => [(Int, p)] -> String
renderActionValues =
    intercalate ", " . fmap renderActionValue
  where
    renderActionValue (actionIndex, value) =
        "#" <> show actionIndex <> ": " <> formatSchedulerValue value

formatSchedulerValue :: RationalOrDouble p => p -> String
formatSchedulerValue value =
    showFFloat (Just 4) (toDouble value) ""

renderAction :: (Show s, Show p) => Action s p -> String
renderAction =
    intercalate "+" . fmap renderOutcome . D.toListD
  where
    renderOutcome ((nextState, cost), prob) =
        show nextState <> "×《" <> show prob <> ", " <> show cost <> "》"

renderTable :: [String] -> [[String]] -> String
renderTable headers rows =
    unlines $ renderRow widths headers : fmap (renderRow widths) rows
  where
    widths =
        fmap (maximum . fmap length) . transpose $ headers : rows

    renderRow ws cols =
        intercalate "  " $ zipWith padRight ws cols

    padRight width s = s <> replicate (max 0 (width - length s)) ' '

initialStateTimeSeries :: ExtremalResult s p -> [Int]
initialStateTimeSeries result = [0 .. erResolvedBudget result]

initialStateCDFSeries :: (Ord s, Num p) => ExtremalResult s p -> ([p], [p])
initialStateCDFSeries result =
    (cdfMin, cdfMax)
  where
    cdfMin = cdfRow (erMinTable result) (erInitialState result) (erResolvedBudget result)
    cdfMax = cdfRow (erMaxTable result) (erInitialState result) (erResolvedBudget result)

initialStatePMFSeries :: (Ord s, Num p) => ExtremalResult s p -> ([p], [p])
initialStatePMFSeries result =
    (pmfFromCDF cdfMin, pmfFromCDF cdfMax)
  where
    (cdfMin, cdfMax) = initialStateCDFSeries result

initialStateRows :: (Ord s, Num p) => ExtremalResult s p -> [(Int, p, p, p, p)]
initialStateRows result =
    zipWith5 rows ts pmfMin pmfMax cdfMin cdfMax
  where
    ts = initialStateTimeSeries result
    (cdfMin, cdfMax) = initialStateCDFSeries result
    (pmfMin, pmfMax) = initialStatePMFSeries result
    rows t pmfMin' pmfMax' cdfMin' cdfMax' = (t, pmfMin', pmfMax', cdfMin', cdfMax')

seriesResultRows :: Num p => ExtremalSeriesResult s p -> [(Int, p, p, p, p)]
seriesResultRows result =
    zipWith5 rows ts pmfMin pmfMax cdfMin cdfMax
  where
    ts = [0 .. esrResolvedBudget result]
    cdfMin = esrCDFMin result
    cdfMax = esrCDFMax result
    pmfMin = pmfFromCDF cdfMin
    pmfMax = pmfFromCDF cdfMax
    rows t pmfMin' pmfMax' cdfMin' cdfMax' = (t, pmfMin', pmfMax', cdfMin', cdfMax')

cdfRow :: Ord s => Num p => ExtremalTable s p -> ConcreteMDPState s -> Int -> [p]
cdfRow table st budget =
    [ tableValue table st t
    | t <- [0 .. budget]
    ]

pmfFromCDF :: Num p => [p] -> [p]
pmfFromCDF [] = []
pmfFromCDF (x : xs) = x : zipWith (-) xs (x : xs)

collectConcreteStates :: Ord s => StateSystem (MDP p) s -> [ConcreteMDPState s]
collectConcreteStates ss =
    Set.toAscList $
        Set.singleton (ssInitial ss)
            <> Set.fromList
                [ (pc, bps)
                | (pc, perState) <- IM.toList (ssTransitions ss)
                , bps <- Map.keys perState
                ]
            <> Set.fromList
                [ next
                | (_, perState) <- IM.toList (ssTransitions ss)
                , (_, mdp) <- Map.toList perState
                , gen <- getGenerators (unMDP mdp)
                , ((next, _), _) <- D.toListD gen
                ]

buildActionMap
    :: Ord s
    => StateSystem (MDP p) s
    -> [ConcreteMDPState s]
    -> Map.Map (ConcreteMDPState s) [Action s p]
buildActionMap ss states =
    Map.fromList
        [ (st, actionGenerators st)
        | st <- states
        ]
  where
    actionGenerators (pc, bps) =
        maybe [] (getGenerators . unMDP) $
            IM.lookup pc (ssTransitions ss) >>= Map.lookup bps

validateExtremalQuery :: ExtremalQuery -> Either String ()
validateExtremalQuery (ExtremalBudget budget)
    | budget < 0 =
        Left "Extremal budget must be non-negative."
    | otherwise =
        Right ()
validateExtremalQuery (ExtremalCoverage target)
    | target < 0 || target > 1 =
        Left "Coverage must lie in the interval [0,1]."
    | otherwise =
        Right ()

validateNonNegativeCosts
    :: (Ord s, Show s)
    => Set.Set (ConcreteMDPState s)
    -> Map.Map (ConcreteMDPState s) [Action s p]
    -> Either String ()
validateNonNegativeCosts goalStates actions =
    case
        [ (st, cost)
        | (st, gens) <- Map.toList actions
        , st `Set.notMember` goalStates
        , gen <- gens
        , ((_, stepCost), _) <- D.toListD gen
        , let cost = getSum (getStepCost stepCost)
        , cost < 0
        ] of
        [] ->
            Right ()
        (st, cost) : _ ->
            Left $
                "The extremal DP solver requires non-negative step costs; "
                    <> "encountered cost " <> show cost <> " in state " <> show st

validateZeroCostAcyclic
    :: (Ord s, Ord p, Num p, Show s)
    => Set.Set (ConcreteMDPState s)
    -> Map.Map (ConcreteMDPState s) [Action s p]
    -> Either String ()
validateZeroCostAcyclic goalStates actions =
    case [cycleStates | CyclicSCC cycleStates <- stronglyConnComp vertices] of
        [] -> Right ()
        cycleStates : _ ->
            Left $
                "The extremal DP solver requires acyclic zero-cost dependencies; "
                    <> "encountered a cycle containing " <> show cycleStates
  where
    vertices =
        [ (st, st, zeroCostSuccessors st gens)
        | (st, gens) <- Map.toList actions
        ]

    zeroCostSuccessors st gens
        | st `Set.member` goalStates = []
        | otherwise =
            [ nextState
            | gen <- gens
            , ((nextState, stepCost), probability) <- D.toListD gen
            , getSum (getStepCost stepCost) == 0
            , probability > 0
            ]

largestPositiveCost
    :: Ord s
    => Set.Set (ConcreteMDPState s)
    -> Map.Map (ConcreteMDPState s) [Action s p]
    -> Int
largestPositiveCost goalStates actions =
    maximum $
        0 :
        [ cost
        | (st, gens) <- Map.toList actions
        , st `Set.notMember` goalStates
        , gen <- gens
        , ((_, stepCost), _) <- D.toListD gen
        , let cost = getSum (getStepCost stepCost)
        , cost > 0
        ]

computeExtremalTable
    :: (Ord s, RationalOrDouble p)
    => TableRetention
    -> ChoiceRetention
    -> ([(Int, Action s p, p)] -> (Int, Action s p, p))
    -> ExtremalQuery
    -> [ConcreteMDPState s]
    -> Set.Set (ConcreteMDPState s)
    -> Map.Map (ConcreteMDPState s) [Action s p]
    -> ConcreteMDPState s
    -> (ExtremalTable s p, Int, Maybe (CoverageStatus p), [SchedulerChoiceTrace s p])
computeExtremalTable tableRetention choiceRetention selectAction query states goalStates actions initialState =
    go 0 0 initialTable Map.empty
  where
    initialTable =
        Map.fromList
            [ (st, IM.empty)
            | st <- states
            ]

    maxPositiveCost = largestPositiveCost goalStates actions

    stabilitySpan = max 1 maxPositiveCost

    retainChoices =
        case choiceRetention of
            RetainChoiceTrace -> True
            DiscardChoiceTrace -> False

    go budget !stableSteps !table !choices =
        let (table', budgetChoices, sameAsPrevious) = appendBudget budget table
            choices' = recordSchedulerChoices choices budgetChoices
            stableSteps' =
                case query of
                    ExtremalBudget _ -> 0
                    ExtremalCoverage _
                        | budget > 0 && sameAsPrevious -> stableSteps + 1
                        | otherwise -> 0
            currentInitial = tableValue table' initialState budget
         in case query of
                ExtremalBudget maxBudget
                    | budget >= maxBudget -> (table', budget, Nothing, schedulerChoiceTraces choices')
                    | otherwise -> go (budget + 1) stableSteps' table' choices'
                ExtremalCoverage target
                    | meetsCoverage target currentInitial ->
                        ( table'
                        , budget
                        , Just (CoverageReached target budget currentInitial)
                        , schedulerChoiceTraces choices'
                        )
                    | stableSteps' >= stabilitySpan ->
                        ( table'
                        , budget
                        , Just (CoverageUnreachable target budget currentInitial)
                        , schedulerChoiceTraces choices'
                        )
                    | otherwise ->
                        go (budget + 1) stableSteps' table' choices'

    appendBudget budget table =
        let cells = foldl' (\memo st -> snd (resolveCell budget table memo st)) Map.empty states
            choicesForBudget =
                if retainChoices
                   then foldMap (maybe [] pure . bcChoice) (Map.elems cells)
                   else []
            sameAsPrevious =
                all
                    (\(st, cell) -> stabilityEqual (bcValue cell) (tableValue table st (budget - 1)))
                    (Map.toList cells)
            expiredBudget = budget - maxPositiveCost - 1
            table' =
                foldl'
                    (\acc (st, cell) -> Map.adjust (updateRow budget expiredBudget st cell) st acc)
                    table
                    (Map.toList cells)
         in (table', choicesForBudget, sameAsPrevious)

    updateRow budget expiredBudget st cell row =
        let row' = IM.insert budget (bcValue cell) row
         in if shouldDiscardExpired st && expiredBudget >= 0
               then IM.delete expiredBudget row'
               else row'
      where
        shouldDiscardExpired state =
            case tableRetention of
                RetainFullTable -> False
                RetainInitialStatePrefix -> state /= initialState
                RetainRollingWindow -> True

    -- Zero-cost dependencies are validated as acyclic before this recursion,
    -- so recursive same-budget evaluation terminates.
    resolveCell budget table memo st
        | Just cell <- Map.lookup st memo = (cell, memo)
        | st `Set.member` goalStates =
            let cell = BudgetCell 1 Nothing
             in (cell, Map.insert st cell memo)
        | otherwise =
            case Map.findWithDefault [] st actions of
                [] ->
                    let cell = BudgetCell 0 Nothing
                     in (cell, Map.insert st cell memo)
                gens ->
                    let (memo', scoredActions) =
                            mapAccumL (scoreAction budget table) memo (zip [1..] gens)
                        (actionIndex, action, value) = selectAction scoredActions
                        actionValues =
                            [ (idx, actionValue)
                            | (idx, _, actionValue) <- scoredActions
                            ]
                        choice =
                            if retainChoices && length gens > 1
                               then Just
                                    ( st
                                    , SchedulerChoice
                                        { scBudget = budget
                                        , scSelection =
                                            if allActionsSameValue scoredActions
                                               then AllActionsSameValue value actionValues
                                               else ChosenAction actionIndex value actionValues action
                                        }
                                    )
                               else Nothing
                        cell = BudgetCell value choice
                     in (cell, Map.insert st cell memo')

    scoreAction budget table memo (actionIndex, action) =
        let (value, memo') =
                foldl'
                    (scoreOutcome budget table)
                    (0, memo)
                    (D.toListD action)
         in (memo', (actionIndex, action, value))

    scoreOutcome budget table (total, memo) ((nextState, cost), prob) =
        let costValue = getSum (getStepCost cost)
         in if costValue == 0
               then
                    let (cell, memo') = resolveCell budget table memo nextState
                     in (total + prob * bcValue cell, memo')
               else
                    ( total + prob * requiredTableValue table nextState (budget - costValue)
                    , memo
                    )

recordSchedulerChoices
    :: Ord s
    => SchedulerChoiceLog s p
    -> [(ConcreteMDPState s, SchedulerChoice s p)]
    -> SchedulerChoiceLog s p
recordSchedulerChoices =
    foldl' recordChoice
  where
    recordChoice logByState (st, choice) =
        Map.alter (Just . appendIfChanged choice) st logByState

    appendIfChanged choice Nothing = [choice]
    appendIfChanged choice (Just []) = [choice]
    appendIfChanged choice (Just existing@(latest:_))
        | sameScheduledAction (scSelection latest) (scSelection choice) = existing
        | otherwise = choice : existing

sameScheduledAction :: SchedulerSelection s p -> SchedulerSelection s p -> Bool
sameScheduledAction (AllActionsSameValue _ _) (AllActionsSameValue _ _) = True
sameScheduledAction (ChosenAction left _ _ _) (ChosenAction right _ _ _) = left == right
sameScheduledAction _ _ = False

schedulerChoiceTraces :: SchedulerChoiceLog s p -> [SchedulerChoiceTrace s p]
schedulerChoiceTraces =
    fmap toTrace . Map.toList
  where
    toTrace (st, choices) =
        SchedulerChoiceTrace
            { sctState = st
            , sctChanges = reverse choices
            }

selectMinAction :: Ord p => [(Int, a, p)] -> (Int, a, p)
selectMinAction = selectActionBy (<)

selectMaxAction :: Ord p => [(Int, a, p)] -> (Int, a, p)
selectMaxAction = selectActionBy (>)

allActionsSameValue :: RationalOrDouble p => [(Int, a, p)] -> Bool
allActionsSameValue [] = True
allActionsSameValue ((_, _, value):xs) =
    all (\(_, _, value') -> approxEqual value value') xs

selectActionBy :: Ord p => (p -> p -> Bool) -> [(Int, a, p)] -> (Int, a, p)
selectActionBy _ [] =
    error "selectActionBy: empty action list"
selectActionBy better (x:xs) =
    foldl' choose x xs
  where
    choose best@(_, _, bestValue) candidate@(_, _, candidateValue)
        | candidateValue `better` bestValue = candidate
        | otherwise = best

tableValue :: Ord s => Num p => ExtremalTable s p -> ConcreteMDPState s -> Int -> p
tableValue _ _ budget | budget < 0 = 0
tableValue table st budget =
    fromMaybe 0 $
        Map.lookup st table >>= IM.lookup budget

requiredTableValue :: Ord s => Num p => ExtremalTable s p -> ConcreteMDPState s -> Int -> p
requiredTableValue _ _ budget | budget < 0 = 0
requiredTableValue table st budget =
    fromMaybe
        (error $ "requiredTableValue: missing retained DP budget " <> show budget)
        (Map.lookup st table >>= IM.lookup budget)

approxEqual :: RationalOrDouble p => p -> p -> Bool
approxEqual x y = abs (toDouble (x - y)) <= 1e-12

-- Approximate equality is not sound here: a small but genuine increment can
-- cross the coverage target at a later budget.  Exact equality is conservative
-- for floating-point probabilities (it may postpone an unreachable verdict),
-- but it cannot create a false one.
stabilityEqual :: Eq p => p -> p -> Bool
stabilityEqual = (==)

meetsCoverage :: RationalOrDouble p => Double -> p -> Bool
meetsCoverage target value = toDouble value >= target
