{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Implementations.MDPExtremal
    ( ConcreteMDPState
    , ExtremalQuery(..)
    , CoverageStatus(..)
    , ExtremalResult(..)
    , computeExtremalReachability
    , renderExtremalResult
    ) where

import qualified Data.Aeson                  as A
import           Data.List                   (foldl', intercalate, transpose, zipWith5)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 (Sum (..))
import qualified Data.IntMap.Strict          as IM
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import           GHC.Exts                    (IsList, Item, toList)

import           BellKAT.Implementations.MDPProbability
    ( MDP(..)
    , StepCost(..)
    )
import           BellKAT.Utils.Automata.Transitions.Functorial (StateSystem(..))
import           BellKAT.Utils.Convex        (getGenerators)
import           BellKAT.Utils.Distribution  (D, RationalOrDouble, toDouble)
import qualified BellKAT.Utils.Distribution  as D

type ConcreteMDPState s = (Int, s)

type ExtremalTable s p = Map.Map (ConcreteMDPState s) (IM.IntMap p)

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
    , erCoverageStatus :: Maybe (CoverageStatus p)
    }
    deriving stock (Eq, Show)

instance (Show p, RationalOrDouble p) => A.ToJSON (CoverageStatus p) where
    toJSON status =
        case status of
            CoverageReached target budget value ->
                coverageToJSON "reached" target budget value
            CoverageUnreachable target budget value ->
                coverageToJSON "unreachable" target budget value
      where
        coverageToJSON :: Show p => String -> Double -> Int -> p -> A.Value
        coverageToJSON kind target budget value =
            A.object
                [ "status" A..= kind
                , "target" A..= target
                , "budget" A..= budget
                , "value" A..= toDouble value
                , "value_exact" A..= show value
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
                , "series_exact" A..=
                    A.object
                        [ "cdf_min" A..= fmap show cdfMin
                        , "cdf_max" A..= fmap show cdfMax
                        ]
                ]

stateToJSON :: (Show s, IsList s, Show (Item s)) => ConcreteMDPState s -> A.Value
stateToJSON st@(pc, bps) =
    A.object
        [ "pc" A..= pc
        , "bell_pairs" A..= fmap show (toList bps)
        , "rendered" A..= show st
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

    validatePositiveCosts goalSet actions

    let (minTable, resolvedBudget, coverageStatus) =
            computeExtremalTable selectMin query states goalSet actions (ssInitial ss)
        (maxTable, _, _) =
            computeExtremalTable selectMax (ExtremalBudget resolvedBudget) states goalSet actions (ssInitial ss)

    pure $
        ExtremalResult
            { erInitialState = ssInitial ss
            , erStates = states
            , erGoalStates = goalStates
            , erResolvedBudget = resolvedBudget
            , erMinTable = minTable
            , erMaxTable = maxTable
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
    -> Map.Map (ConcreteMDPState s) [D p (ConcreteMDPState s, StepCost)]
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

validatePositiveCosts
    :: (Ord s, Show s)
    => Set.Set (ConcreteMDPState s)
    -> Map.Map (ConcreteMDPState s) [D p (ConcreteMDPState s, StepCost)]
    -> Either String ()
validatePositiveCosts goalStates actions =
    case
        [ (st, cost)
        | (st, gens) <- Map.toList actions
        , st `Set.notMember` goalStates
        , gen <- gens
        , ((_, stepCost), _) <- D.toListD gen
        , let cost = getSum (getStepCost stepCost)
        , cost <= 0
        ] of
        [] ->
            Right ()
        (st, cost) : _ ->
            Left $
                "The extremal DP solver currently requires strictly positive step costs; "
                    <> "encountered cost " <> show cost <> " in state " <> show st

computeExtremalTable
    :: (Ord s, RationalOrDouble p)
    => ([p] -> p)
    -> ExtremalQuery
    -> [ConcreteMDPState s]
    -> Set.Set (ConcreteMDPState s)
    -> Map.Map (ConcreteMDPState s) [D p (ConcreteMDPState s, StepCost)]
    -> ConcreteMDPState s
    -> (ExtremalTable s p, Int, Maybe (CoverageStatus p))
computeExtremalTable selectValue query states goalStates actions initialState =
    go 0 0 initialTable
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

    go budget stableSteps table =
        let table' = appendBudget budget table
            stableSteps' =
                if budget > 0 && columnsApproxEqual table' budget (budget - 1)
                   then stableSteps + 1
                   else 0
            currentInitial = tableValue table' initialState budget
         in case query of
                ExtremalBudget maxBudget
                    | budget >= maxBudget -> (table', budget, Nothing)
                    | otherwise -> go (budget + 1) stableSteps' table'
                ExtremalCoverage target
                    | meetsCoverage target currentInitial ->
                        ( table'
                        , budget
                        , Just (CoverageReached target budget currentInitial)
                        )
                    | stableSteps' >= maxObservedCost ->
                        ( table'
                        , budget
                        , Just (CoverageUnreachable target budget currentInitial)
                        )
                    | otherwise ->
                        go (budget + 1) stableSteps' table'

    appendBudget budget table =
        foldl'
            (\acc st -> Map.adjust (IM.insert budget (cellValue table budget st)) st acc)
            table
            states

    cellValue table budget st
        | st `Set.member` goalStates = 1
        | otherwise =
            case Map.findWithDefault [] st actions of
                [] -> 0
                gens ->
                    selectValue
                        [ sum
                            [ prob * tableValue table nextState (budget - getSum (getStepCost cost))
                            | ((nextState, cost), prob) <- D.toListD gen
                            ]
                        | gen <- gens
                        ]

selectMin :: Ord p => [p] -> p
selectMin = minimum

selectMax :: Ord p => [p] -> p
selectMax = maximum

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
