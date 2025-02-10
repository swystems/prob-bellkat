module BellKAT.Implementations.ProbAtomicOneStepQuantum
    ( ProbAtomicOneStepPolicy
    ) where

import Data.List (nub)
import qualified Numeric.Probability.Distribution as P

import qualified BellKAT.Utils.Multiset              as Mset
import BellKAT.Definitions.Core
import BellKAT.Definitions.Structures
import BellKAT.Definitions.Atomic

-- TODO: what's the difference between this design and the one in AtomicOneStepPolicy?
newtype ProbAtomicOneStepPolicy tag = ProbAtomicOneStepPolicy [ProbabilisticAtomicAction tag]
    deriving newtype (Eq, Show)

instance Ord tag => OrderedSemigroup (ProbAtomicOneStepPolicy tag) where
    (ProbAtomicOneStepPolicy xs) <.> (ProbAtomicOneStepPolicy ys) = 
        ProbAtomicOneStepPolicy $ nub $ (<.>) <$> xs <*> ys

instance Ord tag => ParallelSemigroup (ProbAtomicOneStepPolicy tag) where
    (ProbAtomicOneStepPolicy xs) <||> (ProbAtomicOneStepPolicy ys) = 
        ProbAtomicOneStepPolicy $ nub $ (<||>) <$> xs <*> ys

instance Ord tag => CreatesBellPairs (ProbAtomicOneStepPolicy tag) tag where
    tryCreateBellPairFrom (CreateBellPairArgs o i p _) = ProbAtomicOneStepPolicy 
            [ createProbabilitsticAtomicAction 
                (createRestrictedTest mempty) 
                (Mset.fromList i) 
                (P.choose p (Mset.singleton o) mempty)
            , createProbabilitsticAtomicAction 
                (createRestrictedTest  [Mset.fromList i]) 
                mempty 
                (pure mempty)
            ]
