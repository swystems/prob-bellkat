module BellKAT.Utils.Convex.DConvexHull 
    ( reduceConvexHullD
    , isInConvexHullOfD
    ) where

import           Data.Set (Set)
import           Data.Bifunctor
import qualified Data.Map.Strict as Map
import           GHC.Exts (fromList, toList)
import           Data.Array.Comfort.Storable (Array, (!))
import qualified Data.Array.Comfort.Storable as A
import           Data.Array.Comfort.Shape (indices, Range(..), Indexed)
import           Numeric.GLPK

import BellKAT.Utils.Distribution

reduceConvexHullD :: (RationalOrDouble p, Ord p, Ord a) => Set (D p a) -> Set (D p a)
reduceConvexHullD = fromList . reduceDConvexHull' . toList

isInConvexHullOfD :: (RationalOrDouble p, Ord p, Ord a) => D p a -> Set (D p a) -> Bool
x `isInConvexHullOfD` xsSet = 
    let xs = toList xsSet
        sh = computeShape (x:xs)
     in toDVec sh x `isInConvexHullOf'` map (toDVec sh) xs

computeShape :: (Fractional p, Ord p, Ord a) => [D p a] -> Set a
computeShape = foldMap (fromList . map fst . toList)

toArray :: (RationalOrDouble p, Ord p, Ord a) => Set a -> D p a -> Array (Set a) Double
toArray sh = A.fromMap 
    . Map.unionWith (+) (Map.fromSet (const 0) sh) . fromList 
    . map (second toDouble) . toList

toDVec :: (RationalOrDouble p, Ord p, Ord a) => Set a -> D p a -> DVec (Set a) p a
toDVec sh x = (x,  toArray sh x)

toDVecAll :: (RationalOrDouble p, Ord p, Ord a) => [D p a] -> [DVec (Set a) p a]
toDVecAll xs = map (toDVec $ computeShape xs) xs

reduceDConvexHull' :: (RationalOrDouble p, Ord p, Ord a) => [D p a] -> [D p a]
reduceDConvexHull' = reduceDConvexHullV [] . toDVecAll

type DVec sh p a = (D p a, Array sh Double)

reduceDConvexHullV :: Indexed sh => [DVec sh p a] -> [DVec sh p a] -> [D p a]
reduceDConvexHullV acc [] = map fst acc
reduceDConvexHullV acc (x:xs) = 
    if x `isInConvexHullOf'` (acc <> xs)
       then reduceDConvexHullV acc xs
       else reduceDConvexHullV (x:acc) xs

isInConvexHullOf' :: Indexed sh => DVec sh p a -> [DVec sh p a] -> Bool
x `isInConvexHullOf'` xs = snd x `isInConvexHullOf` map snd xs

isInConvexHullOf :: Indexed sh => Array sh Double -> [Array sh Double] -> Bool
_ `isInConvexHullOf` [] = False
x `isInConvexHullOf` xs = 
    case runSimplex x xs of
      Left NoFeasible -> False
      Left e -> error $ "LP error: " <> show e
      Right _ -> True

runSimplex :: Indexed sh => Array sh Double -> [Array sh Double] -> Solution (Range Int)
runSimplex a as =
    let sh = Range 1 (length as)
     in simplex 
            [ i >=. 0 | i <- indices sh] 
            ([ [a' ! i .* x | (x, a') <- zip [1..] as] ==. (a ! i) | i <- indices . A.shape $ a ]
             <> [[1.0 .* j | j <- indices sh]  ==. 1])
            (Maximize, A.fromAssociations 0 sh [])

