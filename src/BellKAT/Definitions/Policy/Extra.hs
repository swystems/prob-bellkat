{-# LANGUAGE OverloadedLists #-}
module BellKAT.Definitions.Policy.Extra 
    ( orderedAsSequence
    ) where

import BellKAT.Definitions.Policy
import           Data.List.NonEmpty         (NonEmpty)

orderedAsSequence :: OrderedStarPolicy a -> Maybe (StarPolicy (NonEmpty a))
orderedAsSequence OSPOne = pure SPOne
orderedAsSequence (OSPStar x) = SPStar <$> orderedAsSequence x
orderedAsSequence (OSPAtomic x) = pure $ SPAtomic [x]
orderedAsSequence (OSPParallel x y) = SPParallel <$> orderedAsSequence x <*> orderedAsSequence y
orderedAsSequence (OSPSequence x y) = SPSequence <$> orderedAsSequence x <*> orderedAsSequence y
orderedAsSequence (OSPChoice x y) = SPChoice <$> orderedAsSequence x <*> orderedAsSequence y
orderedAsSequence (OSPOrdered x y) = 
    case (,) <$> orderedAsSequence x <*> orderedAsSequence y of
      Just (SPAtomic xs, SPAtomic ys) -> pure $ SPAtomic (xs <> ys)
      Just _ -> Nothing
      Nothing -> Nothing
