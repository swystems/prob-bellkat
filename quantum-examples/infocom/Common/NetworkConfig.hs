{- |
    Contains the configuration for the hardware parameters considered in the paper
-}

module Common.NetworkConfig
    ( NetworkParameters(..)
    , defaultNetworkParameters
    , allTopologyNodes
    , repeaterNodes
    , physicalChannelLengths
    , physicalTopologyLinks
    , doublingPathNodes
    , pathElementaryLinks
    , pathPairs
    , topologyDistances
    , lookupDistanceUnits
    , hardwarePGen
    , hardwareW0
    , edgePGen
    , edgeW0
    , withUniformCoherenceTime
    , withUniformSwapProbability
    , withUniformW0
    , networkBoundsFor
    , actionConfigFor
    ) where

import BellKAT.QuantumPrelude hiding (lookup)
import qualified Data.Map.Strict as Map
import GHC.Exts (fromList)

data NetworkParameters = NetworkParameters
    { npReferencePGen :: Double
    , npReferenceW0 :: Double
    , npUniformW0 :: Maybe Double
    , npSwapProbabilities :: Map.Map Location Double
    , npCoherenceTimes :: Map.Map Location Int
    , npEdgeSkew :: Double
    }
    deriving stock (Eq, Show)

-- | Default hardware parameters for the paper topology.
-- Reference p_ge and w0 values are both anchored at the 50 km hardware point.
defaultNetworkParameters :: NetworkParameters
defaultNetworkParameters = NetworkParameters
    { npReferencePGen = hardwarePGen referenceLengthUnits
    , npReferenceW0 = hardwareW0 referenceLengthUnits
    , npUniformW0 = Nothing
    , npSwapProbabilities = Map.fromList
        [ (location, 1 / 2)
        | location <- repeaterNodes
        ]
    , npCoherenceTimes = Map.fromList
        [ (location, 1440000)
        | location <- allTopologyNodes
        ]
    , npEdgeSkew = 1
    }

referenceLengthUnits :: Int
referenceLengthUnits = 5

allTopologyNodes :: [Location]
allTopologyNodes = ["A", "B", "C", "D", "E", "X", "Y", "Z"]

repeaterNodes :: [Location]
repeaterNodes = ["X", "Y", "Z"]

doublingPathNodes :: [Location]
doublingPathNodes = ["A", "X", "Y", "Z", "E"]

pathElementaryLinks :: [(Location, Location)]
pathElementaryLinks = zip doublingPathNodes (tail doublingPathNodes)

pathPairs :: [(Location, Location)]
pathPairs =
    [ (left, right)
    | (index, left) <- zip [(0 :: Int)..] doublingPathNodes
    , right <- drop (index + 1) doublingPathNodes
    ]

-- | Channel lengths are expressed in units of L0 = 10 km
physicalChannelLengths :: [((Location, Location), Int)]
physicalChannelLengths =
    [ (("A", "X"), 2)
    , (("B", "X"), 1)
    , (("X", "Y"), 5)
    , (("Y", "C"), 4)
    , (("Y", "D"), 1)
    , (("Y", "Z"), 5)
    , (("Z", "E"), 1)
    ]

physicalTopologyLinks :: [(Location, Location)]
physicalTopologyLinks = fmap fst physicalChannelLengths

topologyDistances :: [((Location, Location), Int)]
topologyDistances =
    [ ((left, right), distanceBetween left right)
    | (index, left) <- zip [(0 :: Int)..] allTopologyNodes
    , right <- drop (index + 1) allTopologyNodes
    ]

lookupDistanceUnits :: (Location, Location) -> Int
lookupDistanceUnits edge@(left, right) =
    case lookup edge topologyDistances of
        Just distance -> distance
        Nothing ->
            case lookup (right, left) topologyDistances of
                Just distance -> distance
                Nothing -> error $ "missing topology distance for " <> show edge

-- | Probability that a photon traveling in the fiber for L*10km arrives at destination
hardwarePGen :: Int -> Double
hardwarePGen units =
    case units of
        -- with standard telecom loss (attenuation of 0.2db/km)
        -- 1 -> 6.31e-1
        -- 2 -> 3.98e-1
        -- 3 -> 2.51e-1
        -- 4 -> 1.58e-1
        -- 5 -> 1.00e-1
        -- using the parameters from SURF
        1 -> 3.45033332e-3
        2 -> 2.58800000e-3
        3 -> 1.83245824e-3
        4 -> 1.29748965e-3
        5 -> 9.18700000e-4
        _ -> error $ "missing p_gen hardware point for length " <> show (10 * units) <> " km"

hardwareW0 :: Int -> Double
hardwareW0 units =
    case units of
        -- using the parameters from SURF
        1 -> 0.958733
        2 -> 0.957733
        3 -> 0.955956
        4 -> 0.954178
        5 -> 0.952400
        _ -> error $ "missing w0 hardware point for length " <> show (10 * units) <> " km"

edgePGen :: NetworkParameters -> (Location, Location) -> Double
edgePGen parameters edge =
    clamp 0 1 $
        npReferencePGen parameters
        * hardwarePGen distanceUnits
        / hardwarePGen referenceLengthUnits
        / skewPenalty
  where
    distanceUnits = explicitChannelLength edge
    skewPenalty =
        if sameUndirectedEdge edge ("Z", "E")
        then npEdgeSkew parameters
        else 1

edgeW0 :: NetworkParameters -> (Location, Location) -> Double
edgeW0 parameters edge =
    case npUniformW0 parameters of
        Just uniformW0 -> uniformW0
        Nothing ->
            clamp 0 1 $
                npReferenceW0 parameters
                + hardwareW0 distanceUnits
                - hardwareW0 referenceLengthUnits
  where
    distanceUnits = explicitChannelLength edge

withUniformCoherenceTime :: Int -> NetworkParameters -> NetworkParameters
withUniformCoherenceTime coherenceTime parameters =
    parameters
        { npCoherenceTimes = Map.fromList
            [ (location, coherenceTime)
            | location <- allTopologyNodes
            ]
        }

withUniformSwapProbability :: Double -> NetworkParameters -> NetworkParameters
withUniformSwapProbability probability parameters =
    parameters
        { npSwapProbabilities = Map.fromList
            [ (location, probability)
            | location <- repeaterNodes
            ]
        }

withUniformW0 :: Double -> NetworkParameters -> NetworkParameters
withUniformW0 w0 parameters = parameters{npUniformW0 = Just w0}

networkBoundsFor :: [(Location, Location)] -> NetworkBounds QBKATTag
networkBoundsFor capacityPairs =
    def { nbCapacity = Just (fromList [left ~ right | (left, right) <- capacityPairs]) }

actionConfigFor
    :: NetworkParameters
    -> [(Location, Location)]
    -> [Location]
    -> ProbabilisticActionConfiguration
actionConfigFor parameters generationLinks swapNodes = PAC
    { pacTransmitProbability = Map.empty
    , pacCreateProbability = Map.empty
    , pacCreateWerner = Map.empty
    , pacUCreateProbability = Map.fromList
        [ (edge, toRational (edgePGen parameters edge))
        | edge <- generationLinks
        ]
    , pacUCreateWerner = Map.fromList
        [ (edge, edgeW0 parameters edge)
        | edge <- generationLinks
        ]
    , pacSwapProbability = Map.fromList
        [ (location, toRational (lookupSwapProbability parameters location))
        | location <- swapNodes
        ]
    , pacCoherenceTime = npCoherenceTimes parameters
    , pacDistances = Map.fromList topologyDistances
    }

distanceBetween :: Location -> Location -> Int
distanceBetween start goal
    | start == goal = 0
    | otherwise = go Map.empty [(0, start)]
  where
    go _ [] =
        error $ "topology is disconnected between " <> show start <> " and " <> show goal
    go visited queue =
        let ((distance, location), rest) = extractMin queue
        in if Map.member location visited
           then go visited rest
           else
                if location == goal
                then distance
                else
                    let visited' = Map.insert location distance visited
                        next =
                            [ (distance + edgeLength, neighbor)
                            | (neighbor, edgeLength) <- physicalNeighbors location
                            , not (Map.member neighbor visited')
                            ]
                    in go visited' (rest <> next)

physicalNeighbors :: Location -> [(Location, Int)]
physicalNeighbors location =
    [ (right, distance)
    | ((left, right), distance) <- physicalChannelLengths
    , left == location
    ] <>
    [ (left, distance)
    | ((left, right), distance) <- physicalChannelLengths
    , right == location
    ]

extractMin :: [(Int, Location)] -> ((Int, Location), [(Int, Location)])
extractMin [] = error "extractMin called with an empty queue"
extractMin (candidate : candidates) = foldr step (candidate, []) candidates
  where
    step current@(currentDistance, _) (best@(bestDistance, _), rest)
        | currentDistance < bestDistance = (current, best : rest)
        | otherwise = (best, current : rest)

explicitChannelLength :: (Location, Location) -> Int
explicitChannelLength edge@(left, right) =
    case lookup edge physicalChannelLengths of
        Just distance -> distance
        Nothing ->
            case lookup (right, left) physicalChannelLengths of
                Just distance -> distance
                Nothing -> error $ "missing explicit channel length for " <> show edge

lookupSwapProbability :: NetworkParameters -> Location -> Double
lookupSwapProbability parameters location =
    case Map.lookup location (npSwapProbabilities parameters) of
        Just probability -> probability
        Nothing -> error $ "missing swap probability for " <> show location

sameUndirectedEdge :: (Location, Location) -> (Location, Location) -> Bool
sameUndirectedEdge (leftA, rightA) (leftB, rightB) =
    (leftA == leftB && rightA == rightB) || (leftA == rightB && rightA == leftB)

clamp :: Double -> Double -> Double -> Double
clamp lower upper = min upper . max lower
