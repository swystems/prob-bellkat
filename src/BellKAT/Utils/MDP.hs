{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module BellKAT.Utils.MDP
    ( StepCost(..)
    , MDP(..)
    , MDP'
    , fromGenerator
    , fromDistribution
    , singleGenerator
    , successOrFailure
    , parallelCompose
    , setAllCosts
    , mapMDPProbabilities
    , deterministicZeroCostSuccessor
    , minimizeStateSystem
    , primitiveCost
    , combinedRoundCost
    , requireCardinality
    ) where

import           Data.Bifunctor              (first, second)
import qualified Data.IntMap.Strict          as IM
import           Data.List                   (intercalate)
import qualified Data.Map.Strict             as Map
import           Data.Monoid                 (Sum (..))
import qualified Data.Set                    as Set
import           GHC.Exts                    (IsList, fromList, toList)
import           Numeric                     (showFFloat)
import           Control.Subcategory.Bind
import           Control.Subcategory.Functor
import           Control.Subcategory.Pointed

import           BellKAT.Definitions.Core    (Op (..), Probability)
import           BellKAT.Utils.Automata.Transitions.Functorial (StateSystem(..))
import           BellKAT.Utils.Convex
import           BellKAT.Utils.Convex.Constraint
import           BellKAT.Utils.Distribution  (RationalOrDouble)
import qualified BellKAT.Utils.Distribution  as D

newtype StepCost = StepCost { getStepCost :: Sum Int }
    deriving stock (Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance Show StepCost where
    show (StepCost c) = show (getSum c)

mkStepCost :: Int -> StepCost
mkStepCost = StepCost . Sum

newtype MDP p a = MDP { unMDP :: CD p (a, StepCost) }
    deriving newtype (Eq, Ord, Semigroup, Monoid)

type MDP' = MDP Probability

instance Foldable (MDP p) where
    foldMap f = foldMap (f . fst) . unMDP

instance Constrained (MDP p) where
    type Dom (MDP p) a = (DDom a, DDom (a, StepCost))

instance RationalOrDouble p => CPointed (MDP p) where
    cpure = MDP . cpure . \x -> (x, mempty)

instance RationalOrDouble p => CFunctor (MDP p) where
    cmap f = MDP . cmap (first f) . unMDP

instance RationalOrDouble p => CBind (MDP p) where
    cjoin = MDP . cjoin . cmap (\(MDP ma, c) -> cmap (second (c <>)) ma) . unMDP

instance (RealFrac p, Show a) => Show (MDP p a) where
    show (MDP mdp)
        | null gens = "⦅⦆"
        | otherwise = "⦅ " <> intercalate ", " (showGenerator <$> gens) <> " ⦆"
      where
        gens = getGenerators mdp

        showGenerator = intercalate "+" . fmap showOutcome . D.toListD

        showOutcome ((x, c), p) =
            show x <> "×《" <> formatProb p <> ", " <> show c <> "》"

        -- | Show only few fractional digits for readability
        formatProb q = trimTrailingZeros (showFFloat (Just 5) (realToFrac q :: Double) "")

        trimTrailingZeros s =
            let (intPart, fracPartWithDot) = span (/= '.') s
             in case fracPartWithDot of
                    [] -> s
                    (_:fracPart) ->
                        let trimmed = reverse (dropWhile (== '0') (reverse fracPart))
                         in if null trimmed then intPart else intPart <> "." <> trimmed

fromGenerator
    :: (RationalOrDouble p, DDom a, DDom (a, StepCost))
    => [((a, StepCost), p)]
    -> MDP p a
fromGenerator = MDP . fromList . pure . fromList

fromDistribution
    :: (RationalOrDouble p, DDom a, DDom (a, StepCost))
    => D.D p a
    -> MDP p a
fromDistribution =
    MDP . fromList . pure . fmapWithZeroCost
  where
    fmapWithZeroCost = cmap (\st -> (st, mkStepCost 0))

singleGenerator
    :: (RationalOrDouble p, DDom a, DDom (a, StepCost))
    => Int
    -> [a]
    -> MDP p a
singleGenerator cost states =
    fromGenerator
        [ ((st, mkStepCost cost), 1)
        | st <- states
        ]

successOrFailure
    :: (RationalOrDouble p, Monoid a, DDom a, DDom (a, StepCost))
    => Int
    -> Rational
    -> a
    -> MDP p a
successOrFailure cost p successState
    | p == 0 = singleGenerator cost [mempty]
    | p == 1 = singleGenerator cost [successState]
    | otherwise =
        fromGenerator
            [ ((successState, mkStepCost cost), fromRational p)
            , ((mempty,       mkStepCost cost), fromRational (1 - p))
            ]

parallelCompose
    :: (RationalOrDouble p, Semigroup a, DDom a, DDom (a, StepCost))
    => MDP p a
    -> MDP p a
    -> MDP p a
parallelCompose (MDP lhs) (MDP rhs) =
    MDP . fromList $
        [ fromList
            [ ((s1 <> s2, max c1 c2), p1 * p2)
            | ((s1, c1), p1) <- D.toListD d1
            , ((s2, c2), p2) <- D.toListD d2
            ]
        | d1 <- getGenerators lhs
        , d2 <- getGenerators rhs
        ]

setAllCosts
    :: (RationalOrDouble p, DDom a, DDom (a, StepCost))
    => Int
    -> MDP p a
    -> MDP p a
setAllCosts cost (MDP mdp) =
    MDP $ cmap (\(st, _) -> (st, mkStepCost cost)) mdp

mapMDPProbabilities
    :: (RationalOrDouble p, RationalOrDouble p', DDom a, DDom (a, StepCost))
    => (p -> p')
    -> MDP p a
    -> MDP p' a
mapMDPProbabilities f = MDP . D.mapProbability f . unMDP

deterministicZeroCostSuccessor
    :: (Eq p, Num p)
    => MDP p a
    -> Maybe a
deterministicZeroCostSuccessor (MDP mdp) = do
    [generator] <- pure $ getGenerators mdp
    [((next, cost), prob)] <- pure $ D.toListD generator
    if prob == 1 && cost == mempty
       then Just next
       else Nothing

minimizeStateSystem
    :: (DDom s, Eq p, Num p, RationalOrDouble p)
    => StateSystem (MDP p) s
    -> StateSystem (MDP p) s
minimizeStateSystem ss = SS
    { ssInitial = resolveNode (ssInitial ss)
    , ssTransitions = IM.fromListWith Map.union
        [ (pc, Map.singleton st (cmap resolveNode outgoing))
        | (pc, perState) <- IM.toList (ssTransitions ss)
        , (st, outgoing) <- Map.toList perState
        , let node = (pc, st)
        , resolveNode node == node
        ]
    }
  where
    resolveNode node = go Set.empty node
      where
        go seen current
            | current `Set.member` seen = current
            | otherwise =
                case silentSuccessor current of
                    Just next | next /= current -> go (Set.insert current seen) next
                    _ -> current

    silentSuccessor (pc, st) = do
        perState <- IM.lookup pc (ssTransitions ss)
        mdp <- Map.lookup st perState
        deterministicZeroCostSuccessor mdp

primitiveCost :: Op -> Int
primitiveCost FSkip = 0
primitiveCost FDestroy = 0
primitiveCost FCreate{} = 1
primitiveCost (FGenerate _ _ d) = d
primitiveCost (FTransmit _ _ d) = d
primitiveCost (FSwap {}) = 0
-- primitiveCost (FSwap _ _ (d1, d2)) = max d1 d2
primitiveCost (FDistill _ _) = 0
-- primitiveCost (FDistill _ d) = d

-- TODO: we want to refactor this in the DSL
-- if a DSL flag instantaneousOps is enabled by the user,
-- then swap and distill have cost 0
-- otherwise (and default) their cost depend on distances as in the commented code above 


combinedRoundCost :: Foldable t => (a -> Op) -> t a -> Int
combinedRoundCost toOp =
    foldr (max . primitiveCost . toOp) 0

requireCardinality :: IsList s => String -> Int -> s -> a -> a
requireCardinality opName expected bps result
    | actual == expected = result
    | otherwise =
        error $
            "computePrimitiveOutput: " <> opName <> " expected "
            <> show expected <> " inputs, got " <> show actual
  where
    actual = length (toList bps)
