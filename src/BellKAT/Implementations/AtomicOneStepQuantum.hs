{-# LANGUAGE OverloadedLists      #-}

module BellKAT.Implementations.AtomicOneStepQuantum
    ( AtomicOneStepPolicy
    , execute
    ) where

import           Data.List.NonEmpty             (NonEmpty(..))
import           Data.Foldable              (toList)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Default

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Definitions.Core
import BellKAT.Definitions.Tests
import BellKAT.Definitions.Atomic
import BellKAT.Definitions.Structures
import BellKAT.Utils.Choice

newtype AtomicOneStepPolicy tag = AtomicOneStepPolicy (AtomicAction tag)
    deriving newtype (Eq, Ord, Show, OrderedSemigroup, ParallelSemigroup)

execute
    :: (Ord tag)
    => AtomicOneStepPolicy tag
    -> TaggedBellPairs tag
    -> Set (TaggedBellPairs tag)
execute (AtomicOneStepPolicy act) bps =
    if getBPsPredicate (toBPsPredicate . aaTest $ act) bps
       then Set.fromList 
        [ aaOutputBPs act <> rest partial
        | partial <- findElemsND (toList . aaInputBPs $ act) bps]
       else mempty

instance (Ord tag, Default tag) 
  => CreatesBellPairs (NonEmpty (AtomicOneStepPolicy tag)) tag where
    tryCreateBellPairFrom (CreateBellPairArgs bps bp prob _) =
        case prob of
          1.0 -> 
            createBasicAction (Mset.fromList bps) [bp]
          _ ->
              createBasicAction (Mset.fromList bps) [bp]
              <> createBasicAction (Mset.fromList bps) []

instance Ord tag => Tests (AtomicOneStepPolicy tag) FreeTest tag where
    test t = 
        let (s, sig) = getSetAndSign t
         in AtomicOneStepPolicy $
             if sig then
                createAtomicAction (createRestrictedTest mempty) s s
             else
                createAtomicAction (createRestrictedTest [s]) mempty mempty

-- | returns the `TaggedBellPairs` under the test and with which "sign". 
--  * `True` means the test requires _presence_ of `TaggedBellPairs` 
--  * `False` means the test requires _absence_ of `TaggedBellPairs` 
getSetAndSign :: FreeTest tag -> (TaggedBellPairs tag, Bool)
getSetAndSign (FTSubset s) = (s, True)
getSetAndSign (FTNot t) = let (s, sig) = getSetAndSign t in (s, not sig)

createBasicAction 
    :: (Ord tag) 
    => TaggedBellPairs tag -> TaggedBellPairs tag -> NonEmpty (AtomicOneStepPolicy tag)
createBasicAction inBPs outBPs = AtomicOneStepPolicy <$>
    createAtomicAction (createRestrictedTest mempty) inBPs outBPs
    :| createAtomicAction (createRestrictedTest [inBPs]) mempty mempty
    : []
