module BellKAT.Utils.Convex.DConvexHull 
    (reduceDConvexHull) where

import           Data.Set (Set)
import           Data.Bifunctor
import qualified Data.Map.Strict as Map
import           GHC.Exts (fromList, toList)
import           Data.Array.Comfort.Storable (Array, (!))
import qualified Data.Array.Comfort.Storable as A
import           Data.Array.Comfort.Shape (indices, Range(..), Indexed)
import           Numeric.GLPK

import BellKAT.Utils.Distribution

reduceDConvexHull :: Ord a => Set (D a) -> Set (D a)
reduceDConvexHull = fromList . reduceDConvexHull' . toList

reduceDConvexHull' :: Ord a => [D a] -> [D a]
reduceDConvexHull' xs = 
    let xsMaps = map (\x -> (x, Map.fromList $ map (second fromRational) $ toList x)) xs
        allElements = foldMap (Map.keysSet . snd) xsMaps
        xsMapsAll = second (Map.unionWith (+) (Map.fromSet (const 0) allElements)) <$> xsMaps
     in reduceDConvexHullV [] $ second A.fromMap <$> xsMapsAll

type DVec sh a = (D a, Array sh Double)

reduceDConvexHullV :: Indexed sh => [DVec sh a] -> [DVec sh a] -> [D a]
reduceDConvexHullV acc [] = map fst acc
reduceDConvexHullV acc (x:xs) = 
    if x `isInConvexHullOf'` (acc <> xs)
       then reduceDConvexHullV acc xs
       else reduceDConvexHullV (x:acc) xs

isInConvexHullOf' :: Indexed sh => DVec sh a -> [DVec sh a] -> Bool
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

