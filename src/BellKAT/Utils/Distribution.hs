module BellKAT.Utils.Distribution
    ( Probability
    , D
    , choose
    , djoin
    , SD
    , toSubdistribution
    , fromSubdistribution
    , sdjoin
    ) where

import Control.Monad
import Data.List (intercalate)
import GHC.Exts (IsList, Item, fromList, toList)
import Data.Ratio (numerator, denominator)
import Control.Subcategory.Functor
import Control.Subcategory.Applicative
import Control.Subcategory.Pointed

import qualified Numeric.Probability.Distribution as P

type Probability = Rational

newtype D a = D { unD :: P.T Probability a } 

createD :: Ord a => P.T Probability a -> D a
createD = D . P.norm . P.fromFreqs . check . P.decons

check :: [(a, Probability)] -> [(a, Probability)]
check xs = 
    if all ((> 0) . snd) xs && sum (map snd xs) == 1 then xs else error "weird probs"

instance Constrained D where
    type Dom D a = Ord a

instance CFunctor D where
    cmap f = createD . fmap f . unD

instance CPointed D where
    cpure = createD . pure

instance CApplicative D where
    pair (D x) (D y) = createD $ (,) <$> x <*> y
    (D x) <.> (D y) = createD $ x <*> y
    (D x) .> (D y) = createD $ x *> y
    (D x) <. (D y) = createD $ x <* y

toListD :: D a -> [(a, Probability)]
toListD = P.decons . unD

instance Ord a => GHC.Exts.IsList (D a) where
    type Item (D a) = (a, Probability)
    fromList = createD . P.Cons
    toList = toListD

instance (Ord a) => Eq (D a) where
    (D d) == (D d') = P.decons d == P.decons d'

instance Ord a => Ord (D a) where
    (D d) <= (D d') = P.decons d <= P.decons d'

instance Foldable D where
    foldMap f = foldMap (f . fst) . P.decons . unD

showProbability :: Probability -> String
showProbability p = show (numerator p) <> "÷" <> show (denominator p)

instance (Show a) => Show (D a) where
    show = intercalate "+" . map showProb . toListD
      where
        showProb (x, p)
            | p /= 1 = show x <> "×" <> showProbability p
            | otherwise = show x

choose :: Ord a => Probability -> a -> a -> D a
choose p x y = createD $ P.choose p x y

djoin :: Ord a => D (D a) -> D a
djoin =  createD . (unD <=< unD)

newtype SD a = SD { fromSubdistribution :: D (Maybe a) } deriving newtype (Eq, Ord)

instance Ord a => GHC.Exts.IsList (SD a) where
    type Item (SD a) = (a, Probability)
    fromList xs = 
        let totalProbability = sum $ snd <$> xs
         in if totalProbability == 1 
               then SD $ fromList $ [(Just x, p) | (x, p) <- xs]
               else SD $ fromList $ (Nothing, 1 - totalProbability) : [(Just x, p) | (x, p) <- xs]

    toList (SD xs) = [ (x, p) | (Just x, p) <- toListD xs ]
    
instance (Show a) => Show (SD a) where
    show = intercalate "+" . map showProb . toListD . fromSubdistribution
      where
        showProb (x, p)
            | p /= 1 = show x <> "×(" <> show p <> ")"
            | otherwise = show x

instance Constrained SD where
    type Dom SD a = Ord a

instance CFunctor SD where
    cmap f = SD . cmap (fmap f) . fromSubdistribution

instance CPointed SD where
    cpure = SD . cpure . pure

sdjoin :: Ord a => SD (SD a) -> SD a
sdjoin =  SD . djoin . cmap (maybe (cpure Nothing) fromSubdistribution) . fromSubdistribution

toSubdistribution :: Ord a => D a -> SD a
toSubdistribution = SD . cmap Just
