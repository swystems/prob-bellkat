module BellKAT.Utils.Distribution
    ( Probability
    , D
    , norm
    , choose
    ) where

import Data.List (intercalate)
import GHC.Exts (IsList, Item, fromList, toList)

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

instance (Show a, Ord a) => Show (D a) where
    show = intercalate "+" . map showProb . P.decons . P.norm . unD
      where
        showProb (x, p) 
            | p /= 1 = show x <> "×(" <> show p <> ")"
            | otherwise = show x

norm :: Ord a => D a -> D a
norm = D . P.norm . unD

choose :: Probability -> a -> a -> D a
choose p x y = D $ P.choose p x y
