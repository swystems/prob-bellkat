{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{- |
   Module : BellKAT.Implementations.ProbabilisticQuantumOps
   Description : Syntactic definitions related to probabilistic quantum operations
-}
module BellKAT.Implementations.ProbabilisticQuantumOps (
    StateKind(..),
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

data BinaryOutput = BinaryOutput { boOutputBP :: TaggedBellPair (), boOperation :: Op }
    deriving stock (Eq, Ord, Show)

instance Output BinaryOutput where
    type STag BinaryOutput = ()
    type RTag BinaryOutput = StateKind
    type CTag BinaryOutput = ()
    type OutputM BinaryOutput = CostCD'
    computeOutput _ inputBellPairs = cpure inputBellPairs

instance OpOutput BinaryOutput Op where
    fromCBPOutput _ bp op = BinaryOutput { boOutputBP = bp, boOperation = op }
