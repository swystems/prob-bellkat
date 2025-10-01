{-# OPTIONS_GHC -Wno-orphans #-}
module BellKAT.Implementations.UnitTagOps () where

import BellKAT.Definitions.Core
import qualified BellKAT.Utils.Multiset as Mset
import qualified BellKAT.Utils.Distribution as D
import           Data.Foldable              (toList)
import Control.Subcategory.Pointed (cpure)

-- | Trivial interpretation for the unit tag used in tests
instance ValidTag () where
    asFunction FSkip _ = cpure mempty

    asFunction (FCreate p outBp) inBps =
        let emitWithProb q tbp
                | q == 0    = cpure mempty
                | q == 1    = cpure (Mset.singleton tbp)
                | otherwise = D.choose q (Mset.singleton tbp) mempty
        in case toList inBps of
            [] -> emitWithProb p outBp
            _  -> error "FCreate: expected empty input"
    asFunction (FGenerate p outBp) inBps = asFunction (FCreate p outBp) inBps
    asFunction (FTransmit p outBp) inBps =
        let emitWithProb q tbp
                | q == 0    = cpure mempty
                | q == 1    = cpure (Mset.singleton tbp)
                | otherwise = D.choose q (Mset.singleton tbp) mempty
        in case toList inBps of
            [_] -> emitWithProb p outBp
            _   -> error "FTransmit: expected exactly one input pair"
    asFunction (FDestroy _) _ = cpure mempty
    asFunction (FSwap p outBp) inBps =
        let emitWithProb q tbp
                | q == 0    = cpure mempty
                | q == 1    = cpure (Mset.singleton tbp)
                | otherwise = D.choose q (Mset.singleton tbp) mempty
        in case toList inBps of
            [_, _] -> emitWithProb p outBp
            _      -> error "FSwap: expected exactly two input pairs"
    asFunction (FDistill outBp) inBps =
        case toList inBps of
            [_, _] -> cpure (Mset.singleton outBp) -- simplistic deterministic distill
            _      -> error "FDistill: expected exactly two input pairs"
