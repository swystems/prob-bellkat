{-# LANGUAGE ConstraintKinds #-}
module BellKAT.Utils.Convex.Constraint where

import Data.Typeable

-- | caputes all things that can reasonably be stored in probabilistic structures.
-- | Constraints are from the implementation details of the Convex modules and are specified here
-- | for uniformity. 
-- | TODO: can probably be just Ord 
type DDom a = (Show a, Typeable a, Ord a)
