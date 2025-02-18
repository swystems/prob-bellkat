module BellKAT.Utils.Distribution
    ( Probability
    , D
    , norm
    , choose
    , djoin
    , SD
    , toSubdistribution
    , fromSubdistribution
    , sdjoin
    ) where

import Control.Monad
import Control.Applicative
import Data.List (intercalate, intersperse)
import GHC.Exts (IsList, Item, fromList, toList)
import Data.Functor.Classes (Show1(..))
import Data.Ratio (numerator, denominator)

import qualified Numeric.Probability.Distribution as P

type Probability = Rational

newtype D a = D { unD :: P.T Probability a } deriving newtype (Functor, Applicative)

instance GHC.Exts.IsList (D a) where
    type Item (D a) = (a, Probability)
    fromList = D . P.fromFreqs
    toList = P.decons . unD

instance (Ord a) => Eq (D a) where
    (D d) == (D d') = P.equal d d'

instance Ord a => Ord (D a) where
    (D d) <= (D d') = P.decons (P.norm d) <= P.decons (P.norm d')

showProbability :: Probability -> String
showProbability p = show (numerator p) <> "÷" <> show (denominator p)

showsProbability :: Probability -> ShowS
showsProbability p = shows (numerator p) . showString "÷" . shows (denominator p)

instance (Show a, Ord a) => Show (D a) where
    show = intercalate "+" . map showProb . toList . norm
      where
        showProb (x, p)
            | p /= 1 = show x <> "×" <> showProbability p
            | otherwise = show x

instance Show1 D where
    liftShowsPrec aShowsPrec _ _ = mconcat 
        . intersperse (showString "+") . map showProb . toList
      where
        showProb (x, p)
            | p /= 1 = aShowsPrec 0 x . showString "×" . showsProbability p
            | otherwise = aShowsPrec 0 x

norm :: Ord a => D a -> D a
norm = D . P.norm . unD

choose :: Probability -> a -> a -> D a
choose p x y = D $ P.choose p x y

djoin :: D (D a) -> D a
djoin =  D . (unD <=< unD)

newtype SD a = SD { fromSubdistribution :: D (Maybe a) } deriving newtype (Eq, Ord)

instance GHC.Exts.IsList (SD a) where
    type Item (SD a) = (a, Probability)
    fromList xs = 
        let totalProbability = sum $ snd <$> xs
         in if totalProbability == 1 
               then SD $ fromList $ [(Just x, p) | (x, p) <- xs]
               else SD $ fromList $ (Nothing, 1 - totalProbability) : [(Just x, p) | (x, p) <- xs]

    toList (SD xs) = [ (x, p) | (Just x, p) <- toList xs ]
    
instance (Show a, Ord a) => Show (SD a) where
    show = intercalate "+" . map showProb . toList . SD . norm . fromSubdistribution
      where
        showProb (x, p)
            | p /= 1 = show x <> "×(" <> show p <> ")"
            | otherwise = show x

instance Functor SD where
    fmap f = SD . fmap (fmap f) . fromSubdistribution

instance Applicative SD where
    pure = SD . pure . pure
    liftA2 f (SD xs) (SD ys) = SD $ liftA2 (liftA2 f) xs ys

sdjoin :: SD (SD a) -> SD a
sdjoin =  SD . djoin . fmap (maybe (pure Nothing) fromSubdistribution) . fromSubdistribution

toSubdistribution :: D a -> SD a
toSubdistribution = SD . fmap Just
