{- |
   Module : BellKAT.Definitions.QuantumOps
   Description : Syntactic definitions related to quantum operations
-}
module BellKAT.Implementations.QuantumOps (
    -- * Quantum tags
    QuantumTag(..),
    TimeUnit,
    Werner,
    -- * Utils
    getDefaultQuantumBellPair,
    -- * Bell pair operations
    swapBPs,
    distBPs,
) where

import GHC.Exts (fromList, toList)
import qualified BellKAT.Utils.Multiset              as Mset
import Control.Subcategory.Pointed
import Data.Default
import BellKAT.Utils.Distribution as D
import BellKAT.Definitions.Core
import BellKAT.Definitions.Atomic

type TimeUnit = Int      -- discrete and fixed (L/c) time unit
type Werner = Rational   -- representing fidelity, in the range [0,1]

-- | A quantum tag for Bell pairs
data QuantumTag = QuantumTag
    { qtTimestamp  :: TimeUnit -- timestamp of production of the BP
    , qtFidelity :: Werner     -- quality at the time of production of the BP
    }
    deriving stock (Eq, Ord)

instance Show QuantumTag where
    show (QuantumTag t w) = "{w=" ++ show w ++ ", t=" ++ show t ++ "}"

instance Default QuantumTag where
    def = QuantumTag 0 0.8
                     {- ^ example to see fidelity evolving with swap -}

-- | Define an interpretation yeilding quantitatively correct results
instance ValidTag QuantumTag where
    asFunction Skip _ =
        cpure mempty

    asFunction (Try p o) _
            | p == 0 = cpure mempty
            | p == 1 = cpure (Mset.singleton o)
            | otherwise = D.choose p (Mset.singleton o) mempty

    asFunction (Swap p o) chosenBPs =
        swapBPs p chosenBPs o

    asFunction (Distill o) chosenBPs =
        distBPs chosenBPs o


-- | Converts a dummy taggedBP to one with default tags (initial qualities and time of production)
getDefaultQuantumBellPair :: TaggedBellPair tag -> TaggedBellPair QuantumTag
getDefaultQuantumBellPair (TaggedBellPair bp _) = TaggedBellPair bp def

-- | Swap two Bell pairs and returns a distribution D' 
-- | with probability p (the success probability) the output is a new tagged Bell pair connecting the two end nodes, 
-- | and with probability 1-p the swap fails
-- | On failure, no entangled pair remains (both inputs are destroyed in the process). 
-- | Note: it fails if not exactly two bell pairs are given in input
swapBPs :: Rational
            -> TaggedBellPairs QuantumTag
            -> TaggedBellPair QuantumTag
            -> D' (TaggedBellPairs QuantumTag)
swapBPs p inBps (TaggedBellPair outBp _) = 
    case toList inBps of
        [TaggedBellPair _ (QuantumTag t1 w1), TaggedBellPair _ (QuantumTag t2 w2)] ->
            let
                newTag = QuantumTag
                    { qtTimestamp = max t1 t2
                    , qtFidelity  = w1 * w2
                    }
                successOutput = Mset.singleton (TaggedBellPair outBp newTag)
                failureOutput = mempty
            in fromList [ (successOutput, p), (failureOutput, 1 - p) ]
        _ -> error "swapBPs: expected exactly two input tagged Bell pairs"


-- | Perform entanglement distillation on two tagged Bell pairs.
-- | returns a distribution capturing the probabilistic nature of entanglement distillation. 
-- | succeeds with probability `pDist = (1 + wA * wB) / 2`
-- | to yield one new Bell pair with improved fidelity `wDist = (wA + wB + 4 * wA * wB) / (6 * pDist)`
-- | and fails with the remaining probability (yielding no output pair, as the two input pairs are consumed)
-- | Note: it fails if not exactly two bell pairs are given in input
distBPs :: TaggedBellPairs QuantumTag
        -> TaggedBellPair QuantumTag
        -> D' (TaggedBellPairs QuantumTag)
distBPs inBps (TaggedBellPair outBp _) =
    case toList inBps of
        [TaggedBellPair _ (QuantumTag t1 w1), TaggedBellPair _ (QuantumTag t2 w2)] ->
            let
                pDist :: Rational
                pDist = (1 + w1 * w2) / 2
                newTag = QuantumTag
                    { qtTimestamp = max t1 t2
                    , qtFidelity  = (w1 + w2 + 4 * w1 * w2) / (6 * pDist)
                    }
                successOutput = Mset.singleton (TaggedBellPair outBp newTag)
                failureOutput = mempty
            in fromList [ (successOutput, pDist), (failureOutput, 1 - pDist) ]
        _ -> error "distBPs: expected exactly two input tagged Bell pairs"


-- | EXTENSION: Memory decoherence, in function of time, is not yet there
-- | TODO: Since the decaying function is an exponential factor, probably Double would be of enough precision. 
-- | Rational would, in this case, be much more computationally expensive, right?

-- | EXTENSION: Create: Output = Create p BellPair(loc) 
-- representing creation of a new Bell pair at node loc (both ends same), 
-- asFunction will ignore the (empty) input and produce a fresh entangled pair. 
-- With probability p it yields one new TaggedBellPair QuantumTag (the output Bell pair with a new tag), 
-- and with probability 1-p it yields no output (representing failure to create). 
-- The new tag's timestamp is set to a base value (we use 0) since this is a freshly created pair. 
-- Its fidelity is initialized to a default baseline e.g. 1.0

-- | EXTENSION: Transmit: For Output = Transmit p BellPair(locA~locB)
-- representing the transmission of one end of a local pair from src to a remote node dest, 
-- asFunction will consume one input tagged pair (which should be a local loop at src). 
-- On success (probability p), it produces a new entangled pair between dest and src. 
-- The output tag's time is set to (input.qtTimestamp + 1) to model the delay of transmission. 
-- The fidelity of the output pair is kept the same (for now).

-- | EXTENSION: UnstableCreate: Output = UnstableCreate p BellPair(locA~locB) 
-- representing generation (creation and transmission combined) of a new Bell pair. 
-- asFunction will ignore the (empty) input and produce a fresh distributed entangled pair. 
-- With probability p it yields one new TaggedBellPair QuantumTag (the output Bell pair with a new tag), 
-- and with probability 1-p it yields no output (representing failure to generate). 
-- The new tag's timestamp is set to a base value (we use 1) since this is a freshly generated pair. 
-- Its fidelity is initialized to a default baseline e.g. 1.0

-- | EXTENSION: if needed, add Destroy