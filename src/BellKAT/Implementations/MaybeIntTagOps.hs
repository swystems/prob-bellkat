{-# OPTIONS_GHC -Wno-orphans #-}
module BellKAT.Implementations.MaybeIntTagOps () where

import BellKAT.Definitions.Core
import qualified BellKAT.Utils.Multiset as Mset
import qualified BellKAT.Utils.Distribution as D
import           Data.Foldable              (toList)
import Control.Subcategory.Pointed (cpure)

-- | Trivial interpretation for 'Maybe Int' tags used in QuickCheck tests
-- | ignores any tag information
instance ValidTag (Maybe Int) where
    asFunction FSkip _ = cpure mempty

    asFunction (FCreate p outBp) inBps =
        emitWithProbArgs "FCreate: expected empty input" p outBp inBps []

    asFunction (FGenerate p outBp) inBps =
        emitWithProbArgs "FGenerate: expected empty input" p outBp inBps []

    asFunction (FTransmit p outBp) inBps =
        emitWithProbArgs "FTransmit: expected exactly one input pair" p outBp inBps [1]

    asFunction (FDestroy _) _ = cpure mempty

    asFunction (FSwap p outBp) inBps =
        emitWithProbArgs "FSwap: expected exactly two input pairs" p outBp inBps [2]

    asFunction (FDistill outBp) inBps =
        case toList inBps of
            [_, _] -> cpure (Mset.singleton outBp)  -- deterministic success
            _      -> error "FDistill: expected exactly two input pairs"

emitWithProbArgs :: String -> Rational -> TaggedBellPair (Maybe Int)
                    -> TaggedBellPairs (Maybe Int) -> [Int]
                    -> D.D' (TaggedBellPairs (Maybe Int))
emitWithProbArgs errMsg p outBp inBps allowedSizes =
    let sz = length (toList inBps)
    in if null allowedSizes && sz == 0 || sz `elem` allowedSizes
          then emitWithProb p outBp
          else error errMsg
  where
    emitWithProb q tbp
        | q == 0    = cpure mempty
        | q == 1    = cpure (Mset.singleton tbp)
        | otherwise = D.choose q (Mset.singleton tbp) mempty
