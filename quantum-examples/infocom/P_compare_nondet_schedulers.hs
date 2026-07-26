import BellKAT.QuantumPrelude hiding (lookup)
import qualified Common.NondetTopology as Nondet
import qualified Common.NetworkConfig as Net
import Data.List (intercalate, stripPrefix)
import System.Environment (getArgs, withArgs)
import Text.Read (readMaybe)

data Priority
    = PrioritizeAC
    | PrioritizeBD
    deriving stock (Eq, Show)

data Objective
    = ObjectiveAC
    | ObjectiveBD
    | ObjectiveEither
    deriving stock (Eq, Show)

data Scenario = Scenario
    { scProtocolName :: String
    , scPriorityName :: String
    , scObjectiveName :: String
    , scEventName :: String
    , scPGenOverride :: Maybe Double
    , scPSwapOverride :: Maybe Double
    , scW0Override :: Maybe Double
    , scTCohOverride :: Maybe Int
    }

defaultScenario :: Scenario
defaultScenario = Scenario
    { scProtocolName = Nondet.defaultProtocolName
    , scPriorityName = "a-c"
    , scObjectiveName = "a-c"
    , scEventName = "static"
    , scPGenOverride = Nothing
    , scPSwapOverride = Nothing
    , scW0Override = Nothing
    , scTCohOverride = Nothing
    }

priorities :: [(String, Priority)]
priorities =
    [ ("a-c", PrioritizeAC)
    , ("b-d", PrioritizeBD)
    ]

objectives :: [(String, Objective)]
objectives =
    [ ("a-c", ObjectiveAC)
    , ("b-d", ObjectiveBD)
    , ("either", ObjectiveEither)
    ]

availablePriorities :: String
availablePriorities = intercalate ", " (fmap fst priorities)

availableObjectives :: String
availableObjectives = intercalate ", " (fmap fst objectives)

availableEvents :: String
availableEvents = "static, pure, mixed"

selectPriority :: String -> Either String Priority
selectPriority name =
    maybe
        (Left $ "Unknown priority '" <> name <> "'. Available priorities: " <> availablePriorities)
        Right
        (lookup name priorities)

selectObjective :: String -> Either String Objective
selectObjective name =
    maybe
        (Left $ "Unknown objective '" <> name <> "'. Available objectives: " <> availableObjectives)
        Right
        (lookup name objectives)

staticAC :: QBKATTest
staticAC = "A" ~~? "C"

staticBD :: QBKATTest
staticBD = "B" ~~? "D"

pureAC :: QBKATTest
pureAC = "A" -~? "C"

pureBD :: QBKATTest
pureBD = "B" -~? "D"

mixedAC :: QBKATTest
mixedAC = "A" =~? "C"

mixedBD :: QBKATTest
mixedBD = "B" =~? "D"

selectEvent :: Objective -> String -> Either String QBKATTest
selectEvent objective name =
    case (objective, name) of
        (ObjectiveAC, "static") -> Right staticAC
        (ObjectiveBD, "static") -> Right staticBD
        (ObjectiveEither, "static") -> Right $ staticAC ||* staticBD
        (ObjectiveAC, "pure") -> Right pureAC
        (ObjectiveBD, "pure") -> Right pureBD
        (ObjectiveEither, "pure") -> Right $ pureAC ||* pureBD
        (ObjectiveAC, "mixed") -> Right mixedAC
        (ObjectiveBD, "mixed") -> Right mixedBD
        (ObjectiveEither, "mixed") -> Right $ mixedAC ||* mixedBD
        _ -> Left $ "Unknown event '" <> name <> "'. Available events: " <> availableEvents

generations :: QBKATPolicy
generations =
        ucreate ("A", "X")
    <||>
        ucreate ("B", "X")
    <||>
        ucreate ("X", "Y")
    <||>
        ucreate ("C", "Y")
    <||>
        ucreate ("D", "Y")

leftAGuard :: QBKATTest
leftAGuard = hasSubset ["A" ~ "X", "X" ~ "Y"] &&* "A" /~? "Y" &&* "A" /~? "C"

leftBGuard :: QBKATTest
leftBGuard = hasSubset ["B" ~ "X", "X" ~ "Y"] &&* "B" /~? "Y" &&* "B" /~? "D"

leftGoalACGuard :: QBKATTest
leftGoalACGuard = hasSubset ["A" ~ "Y", "C" ~ "Y"] &&* "A" /~? "C"

leftGoalBDGuard :: QBKATTest
leftGoalBDGuard = hasSubset ["B" ~ "Y", "D" ~ "Y"] &&* "B" /~? "D"

leftACBranch :: QBKATPolicy
leftACBranch =
    ite leftAGuard
        (swap "X" ("A", "Y"))
        mempty

leftBDBranch :: QBKATPolicy
leftBDBranch =
    ite leftBGuard
        (swap "X" ("B", "Y"))
        mempty

orderedBranches :: Priority -> QBKATPolicy -> QBKATPolicy -> QBKATPolicy
orderedBranches priority acBranch bdBranch =
    case priority of
        PrioritizeAC -> acBranch <.> bdBranch
        PrioritizeBD -> bdBranch <.> acBranch

chooseLeftBranch :: Priority -> QBKATPolicy
chooseLeftBranch priority =
    orderedBranches priority leftACBranch leftBDBranch

chooseRightEndpoint :: QBKATPolicy
chooseRightEndpoint =
        ite leftGoalACGuard
            (swap "Y" ("A", "C"))
            mempty
    <||>
        ite leftGoalBDGuard
            (swap "Y" ("B", "D"))
            mempty

leftToRightProtocol :: Priority -> QBKATPolicy
leftToRightProtocol priority =
    while Nondet.missingAnyGoal
        ( generations
        <>
          chooseLeftBranch priority
        <>
          chooseRightEndpoint
        )

rightCGuard :: QBKATTest
rightCGuard = hasSubset ["X" ~ "Y", "C" ~ "Y"] &&* "X" /~? "C" &&* "A" /~? "C"

rightDGuard :: QBKATTest
rightDGuard = hasSubset ["X" ~ "Y", "D" ~ "Y"] &&* "X" /~? "D" &&* "B" /~? "D"

rightGoalACGuard :: QBKATTest
rightGoalACGuard = hasSubset ["A" ~ "X", "X" ~ "C"] &&* "A" /~? "C"

rightGoalBDGuard :: QBKATTest
rightGoalBDGuard = hasSubset ["B" ~ "X", "X" ~ "D"] &&* "B" /~? "D"

rightACBranch :: QBKATPolicy
rightACBranch =
    ite rightCGuard
        (swap "Y" ("X", "C"))
        mempty

rightBDBranch :: QBKATPolicy
rightBDBranch =
    ite rightDGuard
        (swap "Y" ("X", "D"))
        mempty

chooseRightBranch :: Priority -> QBKATPolicy
chooseRightBranch priority =
    orderedBranches priority rightACBranch rightBDBranch

chooseLeftEndpoint :: QBKATPolicy
chooseLeftEndpoint =
        ite rightGoalACGuard
            (swap "X" ("A", "C"))
            mempty
    <||>
        ite rightGoalBDGuard
            (swap "X" ("B", "D"))
            mempty

rightToLeftProtocol :: Priority -> QBKATPolicy
rightToLeftProtocol priority =
    while Nondet.missingAnyGoal
        ( generations
        <>
          chooseRightBranch priority
        <>
          chooseLeftEndpoint
        )

schedulerPolicy :: Nondet.ProtocolDirection -> Priority -> QBKATPolicy
schedulerPolicy direction priority =
    case direction of
        Nondet.LeftToRight -> leftToRightProtocol priority
        Nondet.RightToLeft -> rightToLeftProtocol priority

readFlag :: Read a => String -> String -> Either String a
readFlag flag raw =
    case readMaybe raw of
        Nothing -> Left $ "Could not parse " <> flag <> " value '" <> raw <> "'."
        Just value -> Right value

setDouble :: String -> (Double -> Scenario -> Scenario) -> String -> Scenario -> Either String Scenario
setDouble flag setter raw scenario =
    fmap (`setter` scenario) (readFlag flag raw)

setInt :: String -> (Int -> Scenario -> Scenario) -> String -> Scenario -> Either String Scenario
setInt flag setter raw scenario =
    fmap (`setter` scenario) (readFlag flag raw)

stripExampleArgs :: [String] -> Either String (Scenario, [String])
stripExampleArgs = go defaultScenario []
  where
    go scenario kept [] =
        validateScenario scenario *> Right (scenario, reverse kept)
    go _ _ ["--protocol"] = Left "Missing value for --protocol."
    go _ _ ["--priority"] = Left "Missing value for --priority."
    go _ _ ["--objective"] = Left "Missing value for --objective."
    go _ _ ["--event"] = Left "Missing value for --event."
    go _ _ ["--p-gen-override"] = Left "Missing value for --p-gen-override."
    go _ _ ["--p-swap"] = Left "Missing value for --p-swap."
    go _ _ ["--w0-override"] = Left "Missing value for --w0-override."
    go _ _ ["--t-coh"] = Left "Missing value for --t-coh."
    go scenario kept ("--protocol" : name : rest) =
        go scenario{scProtocolName = name} kept rest
    go scenario kept ("--priority" : name : rest) =
        go scenario{scPriorityName = name} kept rest
    go scenario kept ("--objective" : name : rest) =
        go scenario{scObjectiveName = name} kept rest
    go scenario kept ("--event" : name : rest) =
        go scenario{scEventName = name} kept rest
    go scenario kept ("--p-gen-override" : raw : rest) =
        setDouble "--p-gen-override" (\value sc -> sc{scPGenOverride = Just value}) raw scenario >>= \updated ->
            go updated kept rest
    go scenario kept ("--p-swap" : raw : rest) =
        setDouble "--p-swap" (\value sc -> sc{scPSwapOverride = Just value}) raw scenario >>= \updated ->
            go updated kept rest
    go scenario kept ("--w0-override" : raw : rest) =
        setDouble "--w0-override" (\value sc -> sc{scW0Override = Just value}) raw scenario >>= \updated ->
            go updated kept rest
    go scenario kept ("--t-coh" : raw : rest) =
        setInt "--t-coh" (\value sc -> sc{scTCohOverride = Just value}) raw scenario >>= \updated ->
            go updated kept rest
    go scenario kept (arg : rest)
        | Just name <- stripPrefix "--protocol=" arg =
            go scenario{scProtocolName = name} kept rest
        | Just name <- stripPrefix "--priority=" arg =
            go scenario{scPriorityName = name} kept rest
        | Just name <- stripPrefix "--objective=" arg =
            go scenario{scObjectiveName = name} kept rest
        | Just name <- stripPrefix "--event=" arg =
            go scenario{scEventName = name} kept rest
        | Just raw <- stripPrefix "--p-gen-override=" arg =
            setDouble "--p-gen-override" (\value sc -> sc{scPGenOverride = Just value}) raw scenario >>= \updated ->
                go updated kept rest
        | Just raw <- stripPrefix "--p-swap=" arg =
            setDouble "--p-swap" (\value sc -> sc{scPSwapOverride = Just value}) raw scenario >>= \updated ->
                go updated kept rest
        | Just raw <- stripPrefix "--w0-override=" arg =
            setDouble "--w0-override" (\value sc -> sc{scW0Override = Just value}) raw scenario >>= \updated ->
                go updated kept rest
        | Just raw <- stripPrefix "--t-coh=" arg =
            setInt "--t-coh" (\value sc -> sc{scTCohOverride = Just value}) raw scenario >>= \updated ->
                go updated kept rest
        | otherwise =
            go scenario (arg : kept) rest

validateScenario :: Scenario -> Either String ()
validateScenario scenario
    | maybe False invalidProbability (scPGenOverride scenario) =
        Left "--p-gen-override must be a 50 km reference probability in the interval [0, 1]."
    | maybe False invalidProbability (scPSwapOverride scenario) =
        Left "--p-swap must be in the interval [0, 1]."
    | maybe False invalidProbability (scW0Override scenario) =
        Left "--w0-override must be a 50 km reference Werner parameter in the interval [0, 1]."
    | maybe False (<= 0) (scTCohOverride scenario) =
        Left "--t-coh must be positive."
    | otherwise =
        Right ()
  where
    invalidProbability value = value < 0 || value > 1

networkParameters :: Scenario -> Net.NetworkParameters
networkParameters scenario =
    applyTCohOverride
    . applyPSwapOverride
    . applyW0Override
    . applyPGenOverride
    $ Net.defaultNetworkParameters
  where
    applyPGenOverride parameters =
        case scPGenOverride scenario of
            Nothing -> parameters
            Just value -> parameters{Net.npReferencePGen = value}
    applyW0Override parameters =
        case scW0Override scenario of
            Nothing -> parameters
            Just value -> parameters{Net.npReferenceW0 = value}
    applyPSwapOverride parameters =
        case scPSwapOverride scenario of
            Nothing -> parameters
            Just value -> Net.withUniformSwapProbability value parameters
    applyTCohOverride parameters =
        case scTCohOverride scenario of
            Nothing -> parameters
            Just value -> Net.withUniformCoherenceTime value parameters

main :: IO ()
main = do
    args <- getArgs
    (scenario, qbkatArgs) <- either fail pure (stripExampleArgs args)
    direction <- either fail pure (Nondet.selectProtocol (scProtocolName scenario))
    priority <- either fail pure (selectPriority (scPriorityName scenario))
    objective <- either fail pure (selectObjective (scObjectiveName scenario))
    ev <- either fail pure (selectEvent objective (scEventName scenario))
    withArgs qbkatArgs $
        qbkatMainD
            (Nondet.actionConfigFor (networkParameters scenario))
            (Nondet.protocolBounds direction)
            ev
            (schedulerPolicy direction priority)
            mempty
