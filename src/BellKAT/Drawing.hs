{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

module BellKAT.Drawing where

import Diagrams.Prelude hiding (Simple)
import Diagrams.Backend.Cairo (B)

import           Data.Tree (drawForest)
import qualified Data.Tree as T
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (intercalate, intersperse)

import BellKAT.Utils.UnorderedTree (toForest) 
import BellKAT.Definitions.Core
import BellKAT.Definitions.Policy
import BellKAT.Definitions
import BellKAT.Bundles.HistoryBased

pairToDiagram :: (Show t, Eq t, Default t) => TaggedBellPair t -> Diagram B
pairToDiagram (TaggedBellPair bp t) 
  | def == t = (text (show bp) <> rect 4 1) # fontSize (local 0.5) 
  | otherwise = (text (show bp <> "[" <> show t <> "]") <> rect 4 1) # fontSize (local 0.5) 

treeToDiagram t = 
    let childrenNames = [1..(length $ T.subForest t :: Int)]
        rootName = 0 :: Int
        subtrees = zipWith (.>>) childrenNames (map treeToDiagram $ T.subForest t)
        drawEdge = connectOutside' (with & lengths .~ global 0.5)
      in vsep 1 [pairToDiagram (T.rootLabel t) # named rootName, hsep 0.5 subtrees # centerX] 
          # appEndo (mconcat $ map (\i -> Endo $ drawEdge  (i .> rootName) rootName) childrenNames)

frameDiagram d = let d' = d # frame 0.5 in d' <> boundingRect d'

historyToDiagram :: (Tag t, Default t) => History t -> Diagram B
historyToDiagram (History []) = rect 4 0
historyToDiagram (History ts) = hsep 0.5 . map treeToDiagram  . toForest $ ts

historiesToDiagram :: (Tag t, Default t) => [History t] -> Diagram B
historiesToDiagram = vsep 1 . fmap (alignL . frameDiagram . historyToDiagram)

drawPolicy :: (Tag t, Default t) => Simple Policy t -> Diagram B
drawPolicy p = historiesToDiagram . Set.elems . applyPolicy p $ []

drawPolicyTimely :: (Tag t, Default t) => Simple Policy t -> Diagram B
drawPolicyTimely p = historiesToDiagram . Set.elems . applyPolicyTimely p $ []

drawPolicySteps :: (Tag t, Default t) => Simple Policy t -> Diagram B
drawPolicySteps p = historiesToDiagram . Set.elems . applyPolicySteps p $ []

drawOrderedPolicySteps 
    :: (Tag t, Default t) => SeqWithTests Policy BellPairsPredicate t -> Diagram B
drawOrderedPolicySteps p = historiesToDiagram . Set.elems . applyOrderedPolicy p $ []

drawFullOrderedPolicySteps 
    :: (Tag t, Default t) 
    => SeqWithTests FullPolicy BellPairsPredicate t -> Diagram B
drawFullOrderedPolicySteps p = historiesToDiagram . Set.elems . applyFullOrderedPolicy p $ []

drawStarPolicySteps 
    :: (Tag t, Default t) 
    => WithTests OrderedStarPolicy FreeTest t -> Diagram B
drawStarPolicySteps p = historiesToDiagram . Set.elems . applyStarPolicyH p $ []

drawStarPolicyStepsText
    :: (Tag t, Default t) 
    => WithTests OrderedStarPolicy FreeTest t -> String
drawStarPolicyStepsText p = drawHistoriesText . applyStarPolicyH p $ []

drawStarOrderedPolicySteps 
    :: (Tag t, Default t) 
    => WithTests OrderedStarPolicy BellPairsPredicate t -> Diagram B
drawStarOrderedPolicySteps p = historiesToDiagram . Set.elems . applyStarOrderedPolicy p $ []

drawStarOrderedPolicyStepsBounded 
    :: (Tag t, Default t)
    => WithTests OrderedStarPolicy BellPairsPredicate t -> Diagram B
drawStarOrderedPolicyStepsBounded p = historiesToDiagram . Set.elems . applyStarOrderedPolicyBounded p $ []

drawHistoryText :: (Default tag, Show tag, Eq tag) => History tag -> String
drawHistoryText = drawForest . (fmap . fmap) show . toForest . getForest

drawHistoriesText :: (Default tag, Show tag, Eq tag) => Set (History tag) -> String
drawHistoriesText hs = 
    intercalate "\n" $ 
           ["========="]
        <> intersperse "---------" 
            (map (removeFinalEOL . drawHistoryText) . Set.elems $ hs)
        <> ["========="]


removeFinalEOL :: String -> String
removeFinalEOL x
    | not (null x) && last x == '\n' = removeFinalEOL (init x)
    | otherwise =  x
