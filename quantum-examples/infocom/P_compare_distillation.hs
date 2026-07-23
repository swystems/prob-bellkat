import BellKAT.QuantumPrelude hiding (lookup)
import qualified Common.NetworkConfig as Net
import Data.List (intercalate, stripPrefix)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs, withArgs)
import Text.Read (readMaybe)

data ProtocolSpec = ProtocolSpec
    { psPolicy :: QBKATPolicy
    , psGoalTag :: QBKATTag
    }

data Scenario = Scenario
    { scProtocolName :: String
    , scEventName :: String
    , scNetworkParameters :: Net.NetworkParameters
    }

defaultScenario :: Scenario
defaultScenario = Scenario
    { scProtocolName = "swap"
    , scEventName = "pure"
    , scNetworkParameters = Net.defaultNetworkParameters
    }

rawTag :: QBKATTag
rawTag = 0

distilledTag :: QBKATTag
distilledTag = 1

-- physical channels used by both protocols
elementaryLinks :: [(Location, Location)]
elementaryLinks =
    [ ("A", "X")
    , ("X", "Y")
    , ("Y", "C")
    ]

-- X-Y (holding two memory slots) are allowed 
capacityPairs :: [(Location, Location)]
capacityPairs =
    [ ("A", "X")
    , ("X", "Y")
    , ("X", "Y")
    , ("Y", "C")
    , ("X", "C")
    , ("A", "C")
    ]

networkBounds :: NetworkBounds QBKATTag
networkBounds = Net.networkBoundsFor capacityPairs

taggedPair :: QBKATTag -> (Location, Location) -> TaggedBellPair QBKATTag
taggedPair tag (left, right) = (left ~ right) .~ tag

-- p1: keep one raw pair on each elementary link, then swap Y and X.
pSwap :: QBKATPolicy
pSwap =
    while (hasNotSubset [("A" ~ "C") .~ rawTag])
        (
            (   ite (hasNotSubset [("A" ~ "X") .~ rawTag])
                    (ucreate ("A", "X") .~ rawTag)
                    mempty
            <||>
                ite (hasNotSubset [("X" ~ "Y") .~ rawTag])
                    (ucreate ("X", "Y") .~ rawTag)
                    mempty
            <||>
                ite (hasNotSubset [("Y" ~ "C") .~ rawTag])
                    (ucreate ("Y", "C") .~ rawTag)
                    mempty
            )
            <>
            -- Build the right half X-C from X-Y and Y-C.
            ite (hasNotSubset [("X" ~ "C") .~ rawTag])
                (rawTag ~. (swap "Y" ("X", "C") .~ rawTag))
                mempty
            <>
            -- Finish A-C from A-X and X-C.
            rawTag ~. (swap "X" ("A", "C") .~ rawTag)
        )

-- p2: generate A-X and Y-C once, and distill only X-Y before swapping.
pDistSwap :: QBKATPolicy
pDistSwap =
    while (hasNotSubset [("A" ~ "C") .~ distilledTag])
        (
            (   -- The outer links are generated once.  They use the protocol
                -- tag so the current uniform-input-tag swap action can consume
                -- them together with the distilled X-Y pair.
                ite (hasNotSubset [("A" ~ "X") .~ distilledTag])
                    (ucreate ("A", "X") .~ distilledTag)
                    mempty
            <||>
                -- Prepare distilled X-Y.
                ite (hasNotSubset [("X" ~ "Y") .~ distilledTag])
                    (
                        (
                            ite (hasNotSubset [("X" ~ "Y") .~ rawTag, ("X" ~ "Y") .~ rawTag])
                                (ucreate ("X", "Y") .~ rawTag)
                                mempty
                        <||>
                            ite (hasNotSubset [("X" ~ "Y") .~ rawTag])
                                (ucreate ("X", "Y") .~ rawTag)
                                mempty
                        )
                        <>
                        ite (hasSubset [("X" ~ "Y") .~ rawTag, ("X" ~ "Y") .~ rawTag])
                            (distill ("X", "Y") .~ distilledTag)
                            mempty
                    )
                    mempty
            <||>
                -- Generate Y-C once.
                ite (hasNotSubset [("Y" ~ "C") .~ distilledTag])
                    (ucreate ("Y", "C") .~ distilledTag)
                    mempty
            )
            <>
            -- Build the distilled right half X-C.
            ite (hasNotSubset [("X" ~ "C") .~ distilledTag])
                (distilledTag ~. (swap "Y" ("X", "C") .~ distilledTag))
                mempty
            <>
            -- Finish distilled A-C.
            distilledTag ~. (swap "X" ("A", "C") .~ distilledTag)
        )

protocols :: [(String, ProtocolSpec)]
protocols =
    [ ("swap", ProtocolSpec pSwap rawTag)
    , ("p1", ProtocolSpec pSwap rawTag)
    , ("dist-swap", ProtocolSpec pDistSwap distilledTag)
    , ("p2", ProtocolSpec pDistSwap distilledTag)
    ]

availableProtocols :: String
availableProtocols = intercalate ", " (fmap fst protocols)

availableEvents :: String
availableEvents = "static, pure, mixed"

selectProtocol :: String -> Either String ProtocolSpec
selectProtocol name =
    maybe
        (Left $ "Unknown protocol '" <> name <> "'. Available protocols: " <> availableProtocols)
        Right
        (lookup name protocols)

selectEvent :: ProtocolSpec -> String -> Either String QBKATTest
selectEvent protocol name =
    case name of
        "static" -> Right $ hasSubset [goalPair]
        "pure" -> Right $ hasPureSubset [goalPair]
        "mixed" -> Right $ hasMixedSubset [goalPair]
        _ -> Left $ "Unknown event '" <> name <> "'. Available events: " <> availableEvents
  where
    goalPair = taggedPair (psGoalTag protocol) ("A", "C")

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

withParameters :: (Net.NetworkParameters -> Net.NetworkParameters) -> Scenario -> Scenario
withParameters setter scenario =
    scenario{scNetworkParameters = setter (scNetworkParameters scenario)}

setPGen :: Double -> Scenario -> Scenario
setPGen value = withParameters (\parameters -> parameters{Net.npReferencePGen = value})

setW0 :: Double -> Scenario -> Scenario
setW0 value = withParameters (\parameters -> parameters{Net.npReferenceW0 = value})

setUniformW0 :: Double -> Scenario -> Scenario
setUniformW0 value = withParameters (Net.withUniformW0 value)

setPSwap :: Double -> Scenario -> Scenario
setPSwap value = withParameters (Net.withUniformSwapProbability value)

setTCoh :: Int -> Scenario -> Scenario
setTCoh value = withParameters (Net.withUniformCoherenceTime value)

stripExampleArgs :: [String] -> Either String (Scenario, [String])
stripExampleArgs = go defaultScenario []
  where
    go scenario kept [] =
        validateScenario scenario *> Right (scenario, reverse kept)
    go _ _ ["--protocol"] = Left "Missing value for --protocol."
    go _ _ ["--event"] = Left "Missing value for --event."
    go _ _ ["--p-ge"] = Left "Missing value for --p-ge."
    go _ _ ["--p-gen"] = Left "Missing value for --p-gen."
    go _ _ ["--w0"] = Left "Missing value for --w0."
    go _ _ ["--uniform-w0"] = Left "Missing value for --uniform-w0."
    go _ _ ["--p-swap"] = Left "Missing value for --p-swap."
    go _ _ ["--t-coh"] = Left "Missing value for --t-coh."
    go scenario kept ("--protocol" : name : rest) =
        go scenario{scProtocolName = name} kept rest
    go scenario kept ("--event" : name : rest) =
        go scenario{scEventName = name} kept rest
    go scenario kept ("--p-ge" : raw : rest) =
        setDouble "--p-ge" setPGen raw scenario >>= \updated ->
            go updated kept rest
    go scenario kept ("--p-gen" : raw : rest) =
        setDouble "--p-gen" setPGen raw scenario >>= \updated ->
            go updated kept rest
    go scenario kept ("--w0" : raw : rest) =
        setDouble "--w0" setW0 raw scenario >>= \updated ->
            go updated kept rest
    go scenario kept ("--uniform-w0" : raw : rest) =
        setDouble "--uniform-w0" setUniformW0 raw scenario >>= \updated ->
            go updated kept rest
    go scenario kept ("--p-swap" : raw : rest) =
        setDouble "--p-swap" setPSwap raw scenario >>= \updated ->
            go updated kept rest
    go scenario kept ("--t-coh" : raw : rest) =
        setInt "--t-coh" setTCoh raw scenario >>= \updated ->
            go updated kept rest
    go scenario kept (arg : rest)
        | Just name <- stripPrefix "--protocol=" arg =
            go scenario{scProtocolName = name} kept rest
        | Just name <- stripPrefix "--event=" arg =
            go scenario{scEventName = name} kept rest
        | Just raw <- stripPrefix "--p-ge=" arg =
            setDouble "--p-ge" setPGen raw scenario >>= \updated ->
                go updated kept rest
        | Just raw <- stripPrefix "--p-gen=" arg =
            setDouble "--p-gen" setPGen raw scenario >>= \updated ->
                go updated kept rest
        | Just raw <- stripPrefix "--w0=" arg =
            setDouble "--w0" setW0 raw scenario >>= \updated ->
                go updated kept rest
        | Just raw <- stripPrefix "--uniform-w0=" arg =
            setDouble "--uniform-w0" setUniformW0 raw scenario >>= \updated ->
                go updated kept rest
        | Just raw <- stripPrefix "--p-swap=" arg =
            setDouble "--p-swap" setPSwap raw scenario >>= \updated ->
                go updated kept rest
        | Just raw <- stripPrefix "--t-coh=" arg =
            setInt "--t-coh" setTCoh raw scenario >>= \updated ->
                go updated kept rest
        | otherwise =
            go scenario (arg : kept) rest

validateScenario :: Scenario -> Either String ()
validateScenario scenario
    | Net.npReferencePGen parameters <= 0 || Net.npReferencePGen parameters > 1 =
        Left "--p-ge/--p-gen must be a 50 km reference probability in the interval (0, 1]."
    | Net.npReferenceW0 parameters < 0 || Net.npReferenceW0 parameters > 1 =
        Left "--w0 must be a 50 km reference Werner parameter in the interval [0, 1]."
    | maybe False invalidProbability (Net.npUniformW0 parameters) =
        Left "--uniform-w0 must be in the interval [0, 1]."
    | any invalidProbability (Map.elems (Net.npSwapProbabilities parameters)) =
        Left "--p-swap must be in the interval [0, 1]."
    | any (<= 0) (Map.elems (Net.npCoherenceTimes parameters)) =
        Left "--t-coh must be positive."
    | otherwise =
        Right ()
  where
    parameters = scNetworkParameters scenario
    invalidProbability value = value < 0 || value > 1

actionConfig :: Scenario -> ProbabilisticActionConfiguration
actionConfig scenario =
    Net.actionConfigFor (scNetworkParameters scenario) elementaryLinks ["X", "Y"]

main :: IO ()
main = do
    args <- getArgs
    (scenario, qbkatArgs) <- either fail pure (stripExampleArgs args)
    protocol <- either fail pure (selectProtocol (scProtocolName scenario))
    ev <- either fail pure (selectEvent protocol (scEventName scenario))
    withArgs qbkatArgs $
        qbkatMainD (actionConfig scenario) networkBounds ev (psPolicy protocol) mempty
