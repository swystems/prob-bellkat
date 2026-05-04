{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
   Module : BellKAT.Implementations.ProbabilisticQuantumOps
   Description : Syntactic definitions related to probabilistic quantum operations
-}
module BellKAT.Implementations.ProbabilisticQuantumOps (
    DistillationCount,
    StateKind(..),
    WernerTag(..),
    BinaryOutput(..)
) where

import Control.Subcategory.Pointed
import Data.Default

import BellKAT.Definitions.Core
import BellKAT.Implementations.Output
import BellKAT.Utils.Cost

data StateKind = Pure | Mixed
    deriving stock (Eq, Ord)

instance Show StateKind where
    show Pure  = "1"
    show Mixed = "0"

instance Default StateKind where
    def = Mixed

instance RuntimeTag StateKind () where
  staticTag _ = ()

type DistillationCount = Int

data WernerTag = WernerTag
    { wtDistillations :: DistillationCount
    , wtStateKind     :: StateKind
    } deriving stock (Eq, Ord)

instance Show WernerTag where
    show (WernerTag n kind) = show kind <> "@" <> show n

instance Default WernerTag where
    def = WernerTag def def

instance RuntimeTag WernerTag DistillationCount where
    staticTag = wtDistillations

data BinaryOutput = BinaryOutput { boOutputBP :: TaggedBellPair DistillationCount, boOperation :: Op }
    deriving stock (Eq, Ord, Show)

instance Output BinaryOutput where
    type STag BinaryOutput = DistillationCount
    type RTag BinaryOutput = WernerTag
    type CTag BinaryOutput = ()
    type OutputM BinaryOutput = CostCD'
    computeOutput _ inputBellPairs = cpure inputBellPairs

instance OpOutput BinaryOutput Op where
    fromCBPOutput _ bp op = BinaryOutput { boOutputBP = bp, boOperation = op }
