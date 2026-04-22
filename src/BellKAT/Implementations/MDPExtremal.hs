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
    , computeExtremalReachability
    , renderExtremalResult
    ) where

import qualified Data.Aeson                  as A
import           Data.List                   (foldl', intercalate, mapAccumL, transpose, zipWith5)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 (Sum (..))
import qualified Data.IntMap.Strict          as IM
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import           GHC.Exts                    (IsList, Item, toList)

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
    = ChosenAction Int p (Action s p)
    | AllActionsSameValue p
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
            ChosenAction actionIndex value action ->
                [ "kind" A..= ("chosen_action" :: String)
                , "action_index" A..= actionIndex
                , "value" A..= toDouble value
                , "action" A..= renderAction action
                ]
            AllActionsSameValue value ->
                [ "kind" A..= ("all_actions_same_value" :: String)
                , "value" A..= toDouble value
                ]

computeExtremalReachability
    :: (Ord s, Show s, RationalOrDouble p)
    => (s -> Bool)
    -> ExtremalQuery
    -> StateSystem (MDP p) s
    -> Either String (ExtremalResult s p)
computeExtremalReachability isGoal query ss = do
    validateExtremalQuery query
    let states = collectConcreteStates ss
        goalStates = filter (isGoal . snd) states
        goalSet = Set.fromList goalStates
        actions = buildActionMap ss states

    validateNonNegativeCosts goalSet actions

    let (minTable, resolvedBudget, coverageStatus, minChoices) =
            computeExtremalTable selectMinAction query states goalSet actions (ssInitial ss)
        (maxTable, _, _, maxChoices) =
            computeExtremalTable selectMaxAction (ExtremalBudget resolvedBudget) states goalSet actions (ssInitial ss)

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

renderExtremalResult :: (Ord s, Num p, Show p, Show s) => ExtremalResult s p -> String
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

renderSchedulerChoices :: (Show s, Show p) => String -> [SchedulerChoiceTrace s p] -> String
renderSchedulerChoices title [] =
    unlines [title, "  none"]
renderSchedulerChoices title traces =
    unlines $ title : concatMap renderSchedulerChoiceTrace traces

renderSchedulerChoiceTrace :: (Show s, Show p) => SchedulerChoiceTrace s p -> [String]
renderSchedulerChoiceTrace trace =
    ("  state=" <> show (sctState trace))
        : fmap (("    " <>) . renderSchedulerChoice) (sctChanges trace)

renderSchedulerChoice :: (Show s, Show p) => SchedulerChoice s p -> String
renderSchedulerChoice choice =
    "from t=" <> show (scBudget choice)
        <> case scSelection choice of
            ChosenAction actionIndex value action ->
                ": choose action #" <> show actionIndex
                    <> " with value " <> show value
                    <> " -> " <> renderAction action
            AllActionsSameValue value ->
                ": all actions have same value " <> show value

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

computeExtremalTable
    :: (Ord s, RationalOrDouble p)
    => ([(Int, Action s p, p)] -> (Int, Action s p, p))
    -> ExtremalQuery
    -> [ConcreteMDPState s]
    -> Set.Set (ConcreteMDPState s)
    -> Map.Map (ConcreteMDPState s) [Action s p]
    -> ConcreteMDPState s
    -> (ExtremalTable s p, Int, Maybe (CoverageStatus p), [SchedulerChoiceTrace s p])
computeExtremalTable selectAction query states goalStates actions initialState =
    go 0 0 initialTable Map.empty
  where
    initialTable =
        Map.fromList
            [ (st, IM.empty)
            | st <- states
            ]

    maxObservedCost =
        maximum $
            1 :
            [ getSum (getStepCost stepCost)
            | gens <- Map.elems actions
            , gen <- gens
            , ((_, stepCost), _) <- D.toListD gen
            ]

    go budget stableSteps table choices =
        let (table', budgetChoices) = appendBudget budget table
            choices' = recordSchedulerChoices choices budgetChoices
            stableSteps' =
                if budget > 0 && columnsApproxEqual table' budget (budget - 1)
                   then stableSteps + 1
                   else 0
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
                    | stableSteps' >= maxObservedCost ->
                        ( table'
                        , budget
                        , Just (CoverageUnreachable target budget currentInitial)
                        , schedulerChoiceTraces choices'
                        )
                    | otherwise ->
                        go (budget + 1) stableSteps' table' choices'

    appendBudget budget table =
        let cells = foldl' (\memo st -> snd (resolveCell budget table memo st)) Map.empty states
            choicesForBudget = foldMap (maybe [] pure . bcChoice) (Map.elems cells)
            table' =
                foldl'
                    (\acc (st, cell) -> Map.adjust (IM.insert budget (bcValue cell)) st acc)
                    table
                    (Map.toList cells)
         in (table', choicesForBudget)

    -- TODO: zero-cost dependencies are assumed acyclic. Under that user-side
    -- precondition, recursive same-budget evaluation terminates; zero-cost
    -- loops should be rejected by a future validation pass.
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
                        choice =
                            if length gens > 1
                               then Just
                                    ( st
                                    , SchedulerChoice
                                        { scBudget = budget
                                        , scSelection =
                                            if allActionsSameValue scoredActions
                                               then AllActionsSameValue value
                                               else ChosenAction actionIndex value action
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
                    ( total + prob * tableValue table nextState (budget - costValue)
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
sameScheduledAction (AllActionsSameValue _) (AllActionsSameValue _) = True
sameScheduledAction (ChosenAction left _ _) (ChosenAction right _ _) = left == right
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

columnsApproxEqual :: (Ord s, RationalOrDouble p) => ExtremalTable s p -> Int -> Int -> Bool
columnsApproxEqual table left right =
    all approxEntry (Map.keys table)
  where
    approxEntry st = approxEqual (tableValue table st left) (tableValue table st right)

approxEqual :: RationalOrDouble p => p -> p -> Bool
approxEqual x y = abs (toDouble (x - y)) <= 1e-12

meetsCoverage :: RationalOrDouble p => Double -> p -> Bool
meetsCoverage target value = toDouble value >= target
