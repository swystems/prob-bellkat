{-# OPTIONS_GHC -Wno-missing-signatures #-}

module BellKAT.Test (memoryBounds) where

import           GHC.Exts (toList)
import           Data.Map.Strict (Map)
import           Data.Default
import           BellKAT.Definitions

countQubitsAtLocationBP :: (Default tag, Ord tag) => Location -> TaggedBellPair tag -> Int
countQubitsAtLocationBP l tbp = let (x , y) = locations tbp in length $ filter (== l) [x, y]

countQubitsAtLocation :: (Default tag, Ord tag) => Location -> TaggedBellPairs tag -> Int
countQubitsAtLocation l = sum . map (countQubitsAtLocationBP l) . toList

-- | produces a predicate over valid states from upper bounds on the number of qubits at certain
-- locations. The state is valid if and only if /all/ the bounds are satisfied.
--
-- For instance no constraints mean \"always true\" predicate:
--
--  @'memoryBounds' [] = 'const' True@
--
-- To impose an upper bound @3@ on the number of qubits at location @\"A\"@ use (assuming
-- 'OverloadedLists' and 'OverloadedStrings' extensions:
--
--  @'memoryBounds' [(\"A\", 3)]@
memoryBounds 
    :: (Default tag, Ord tag)
    => Map Location Int -- ^ Upper bounds on the number of qubits at respective locations
    -> TaggedBellPairs tag -> Bool
memoryBounds bounds bps = all (\(l,k) -> countQubitsAtLocation l bps <= k) $ toList bounds
