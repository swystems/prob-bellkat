{-# LANGUAGE OverloadedStrings #-}
module BellKAT.Utils.Distribution
    ( Probability
    , D
    , D'
    , toListD
    , choose
    , djoin
    , computeEventProbability
    , SD
    , SD'
    , toSubdistribution
    , fromSubdistribution
    , sdjoin
    , HasMapProbability (..)
    , RationalOrDouble (..)
    ) where

import Control.Monad
import Data.Bifunctor
import Data.List (intercalate)
import GHC.Exts (IsList, Item, fromList, toList)
import Control.Subcategory.Functor
import Control.Subcategory.Applicative
import Control.Subcategory.Pointed
import Data.Typeable
import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))

import qualified Numeric.Probability.Distribution as P

type Probability = Rational

newtype D p a = D { unD :: P.T p a } 
type D' = D Probability

createD :: (Fractional p, Ord p, Ord a) => P.T p a -> D p a
createD = D . P.norm . P.fromFreqs . check . P.decons

-- TODO: should be zero for Rational
errorMargin :: (Fractional p) => p
errorMargin = 1 / (10 ^ (9 :: Int))

check :: (Ord p, Fractional p) => [(a, p)] -> [(a, p)]
check xs = 
    if all ((> 0) . snd) xs && abs (sum (map snd xs) - 1) < errorMargin then xs else error "weird probs"

instance Constrained (D p) where
    type Dom (D p) a = Ord a

instance (Fractional p, Ord p) => CFunctor (D p) where
    cmap f = createD . fmap f . unD

instance (Fractional p, Ord p) => CPointed (D p) where
    cpure = createD . pure

instance (Fractional p, Ord p) => CApplicative (D p) where
    pair (D x) (D y) = createD $ (,) <$> x <*> y
    (D x) <.> (D y) = createD $ x <*> y
    (D x) .> (D y) = createD $ x *> y
    (D x) <. (D y) = createD $ x <* y

toListD :: D p a -> [(a, p)]
toListD = P.decons . unD

instance (Fractional p, Ord p, Ord a) => GHC.Exts.IsList (D p a) where
    type Item (D p a) = (a, p)
    fromList = createD . P.Cons
    toList = toListD

instance (Ord a, Eq p) => Eq (D p a) where
    (D d) == (D d') = P.decons d == P.decons d'

instance (Ord p, Ord a) => Ord (D p a) where
    (D d) <= (D d') = P.decons d <= P.decons d'

instance Foldable (D p) where
    foldMap f = foldMap (f . fst) . P.decons . unD

instance (Eq p, Num p, Show p, Show a) => Show (D p a) where
    show = intercalate "+" . map showProb . toListD
      where
        showProb (x, p)
            | p /= 1 = show x <> "×" <> show p
            | otherwise = show x

choose :: (Fractional p, Ord p, Ord a) => p -> a -> a -> D p a
choose p x y = createD $ P.choose p x y

djoin :: (Fractional p, Ord p, Ord a) => D p (D p a) -> D p a
djoin =  createD . (unD <=< unD)

computeEventProbability :: Num p => (a -> Bool) -> D p a -> p
computeEventProbability ev = sum . map snd . filter (ev . fst) . toListD

newtype SD p a = SD { fromSubdistribution :: D p (Maybe a) } deriving newtype (Eq, Ord)
type SD' = SD Probability

instance (Fractional p, Ord p, Ord a) => GHC.Exts.IsList (SD p a) where
    type Item (SD p a) = (a, p)
    fromList xs = 
        let totalProbability = sum $ snd <$> xs
         in if totalProbability == 1 
               then SD $ fromList $ [(Just x, p) | (x, p) <- xs]
               else SD $ fromList $ (Nothing, 1 - totalProbability) : [(Just x, p) | (x, p) <- xs]

    toList (SD xs) = [ (x, p) | (Just x, p) <- toListD xs ]
    
instance (Show a, Eq p, Num p, Show p) => Show (SD p a) where
    show = intercalate "+" . map showProb . toListD . fromSubdistribution
      where
        showProb (x, p)
            | p /= 1 = show x <> "×(" <> show p <> ")"
            | otherwise = show x

instance Constrained (SD p) where
    type Dom (SD p) a = Ord a

instance (Fractional p, Ord p) => CFunctor (SD p) where
    cmap f = SD . cmap (fmap f) . fromSubdistribution

instance (Fractional p, Ord p) => CPointed (SD p) where
    cpure = SD . cpure . pure

sdjoin :: (Fractional p, Ord p, Ord a) => SD p (SD p a) -> SD p a
sdjoin =  SD . djoin . cmap (maybe (cpure Nothing) fromSubdistribution) . fromSubdistribution

toSubdistribution :: (Fractional p, Ord p, Ord a) => D p a -> SD p a
toSubdistribution = SD . cmap Just

class (Typeable p, Show p, RealFrac p) => RationalOrDouble p where
    toDouble :: p -> Double

instance RationalOrDouble Rational where
    toDouble = fromRational

instance RationalOrDouble Double where
    toDouble = id

class HasMapProbability t where
    mapProbability :: (RationalOrDouble p, RationalOrDouble p', Show a, Typeable a, Ord a) => (p -> p') -> t p a -> t p' a

instance HasMapProbability D where
    mapProbability f = createD . P.fromFreqs . map (second f) . P.decons .  unD

instance (A.ToJSON a, A.ToJSON p, Ord a, RationalOrDouble p) => A.ToJSON (D p a) where
    toJSON = A.toJSON . fmap probabilityTermToJSON . toList
      where 
        probabilityTermToJSON (x, p) = A.object ["value" .= x, "probability" .= p]

instance (A.FromJSON a, A.FromJSON p, Ord a, RationalOrDouble p) => A.FromJSON (D p a) where
    parseJSON = fmap fromList . mapM parseProbabilityTerm <=< A.parseJSON
      where 
        parseProbabilityTerm = A.withObject "probTerm" $ 
            \v -> (,) <$> v .: "value" <*> v .: "probability"
