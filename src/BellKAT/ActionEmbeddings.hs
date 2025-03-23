module BellKAT.ActionEmbeddings 
    ( simpleActionMeaning
    , ProbabilisticActionConfiguration(..)
    , probabilisticActionMeaning
    , CanDesugarActions(..)
    , mapDesugarActions
    ) where

import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import BellKAT.Definitions.Policy
import BellKAT.Definitions.Core

class CanDesugarActions a where
    type Tag a :: Type
    type Desugared a :: Type
    desugarActions :: (TaggedAction (Tag a) -> CreateBellPairArgs (Tag a)) -> a -> Desugared a

mapDesugarActions 
    :: (Functor f, CanDesugarActions a)
    => (TaggedAction (Tag a) -> CreateBellPairArgs (Tag a)) -> f a -> f (Desugared a)
mapDesugarActions = fmap . desugarActions

simpleActionMeaning :: TaggedAction t -> CreateBellPairArgs t
simpleActionMeaning ta = case taAction ta of
    (Swap l (l1, l2))     -> CreateBellPairArgs
        [l ~ l1 @ taTagIn ta, l ~ l2 @ taTagIn ta] (l1 ~ l2 @ taTagOut ta)
        1.0 (taDup ta)
    (Transmit l (l1, l2)) -> CreateBellPairArgs
        [l ~ l @ taTagIn ta] (l1 ~ l2 @ taTagOut ta)
        1.0 (taDup ta)
    (Create l)            -> CreateBellPairArgs
        [] (l ~ l @ taTagOut ta ) 1.0 (taDup ta)
    (Destroy (l1, l2))            -> CreateBellPairArgs
        [l1 ~ l2 @ taTagIn ta] (l1 ~ l2 @ taTagOut ta) 0.0 (taDup ta)
    (Distill (l1, l2))    -> CreateBellPairArgs
        [l1 ~ l2 @ taTagIn ta, l1 ~ l2  @ taTagIn ta] (l1 ~ l2 @ taTagOut ta )
        0.5 (taDup ta)
    (UnstableCreate (l1, l2)) -> CreateBellPairArgs
        [] (l1 ~ l2 @ taTagOut ta ) 0.5 (taDup ta)

data ProbabilisticActionConfiguration = PAC 
    { pacTransmitProbability :: Map (Location, Location) Probability
    , pacCreateProbability :: Map Location Probability
    , pacUCreateProbability :: Map (Location, Location) Probability
    , pacSwapProbability :: Map Location Probability
    }

probabilisticActionMeaning :: ProbabilisticActionConfiguration -> TaggedAction t -> CreateBellPairArgs t
probabilisticActionMeaning pac ta = case taAction ta of
    (Swap l (l1, l2))     -> CreateBellPairArgs
        [l ~ l1 @ taTagIn ta, l ~ l2 @ taTagIn ta] (l1 ~ l2 @ taTagOut ta)
        (swapProbability pac l) (taDup ta)
    (Transmit l (l1, l2)) -> CreateBellPairArgs
        [l ~ l @ taTagIn ta] (l1 ~ l2 @ taTagOut ta)
        (transmitProbability pac l (l1, l2)) (taDup ta)
    (Create l)            -> CreateBellPairArgs
        [] (l ~ l @ taTagOut ta ) (createProbability pac l) (taDup ta)
    (Destroy (l1, l2))            -> CreateBellPairArgs
        [l1 ~ l2 @ taTagIn ta] (l1 ~ l2 @ taTagOut ta) 0 (taDup ta)
    (Distill (l1, l2))    -> CreateBellPairArgs
        [l1 ~ l2 @ taTagIn ta, l1 ~ l2  @ taTagIn ta] (l1 ~ l2 @ taTagOut ta)  
        0.5 (taDup ta)
    (UnstableCreate (l1, l2)) -> CreateBellPairArgs
        [] (l1 ~ l2 @ taTagOut ta) (uCreateProbability pac (l1, l2)) (taDup ta)

createProbability :: ProbabilisticActionConfiguration -> Location -> Probability
createProbability pac l = 
    case pacCreateProbability pac Map.!? l of
      Nothing -> error $ "no create probability for " <> show l
      Just p -> p

swapProbability :: ProbabilisticActionConfiguration -> Location -> Probability
swapProbability pac l = 
    case pacSwapProbability pac Map.!? l of
      Nothing -> error $ "no swap probability for " <> show l
      Just p -> p

uCreateProbability :: ProbabilisticActionConfiguration -> (Location, Location) -> Probability
uCreateProbability pac l =
    case pacUCreateProbability pac Map.!? l of
      Nothing -> error $ "no ucreate probability for " <> show l
      Just p -> p

transmitProbability :: ProbabilisticActionConfiguration -> Location -> (Location, Location) -> Probability
transmitProbability pac l (l1, l2) = 
    let p1 = if l1 == l then 1 else transmitProbabilitySingle pac (l, l1)
        p2 = if l2 == l then 1 else transmitProbabilitySingle pac (l, l2)
     in p1 * p2

transmitProbabilitySingle :: ProbabilisticActionConfiguration -> (Location, Location) -> Probability
transmitProbabilitySingle pac l = 
    case pacTransmitProbability pac Map.!? l of
      Nothing -> error $ "no transmit probability for " <> show l
      Just p -> p

instance CanDesugarActions (TaggedAction tag) where
    type Tag (TaggedAction tag) = tag
    type Desugared (TaggedAction tag) = CreateBellPairArgs tag
    desugarActions = id

instance CanDesugarActions a => CanDesugarActions (NonEmpty a) where
    type Tag (NonEmpty a) = Tag a
    type Desugared (NonEmpty a) = NonEmpty (Desugared a)
    desugarActions = fmap . desugarActions

instance CanDesugarActions (Atomic TaggedAction test tag) where 
    type Tag (Atomic TaggedAction test tag) = tag
    type Desugared (Atomic TaggedAction test tag) = (Atomic CreateBellPairArgs test tag)
    desugarActions f (AAction x) = AAction (f x)
    desugarActions _ (ATest t) = ATest t
