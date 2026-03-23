{-# LANGUAGE OverloadedStrings #-}
module BellKAT.Bundles.Desugaring
    ( desugarStage
    , probabilisticDesugarStage
    ) where

import           BellKAT.ActionEmbeddings
import           BellKAT.Bundles.Core

-- | A stage that desugars actions within a functorial structure using a simple interpretation.
--   It applies the 'simpleActionMeaning' to each action.
desugarStage
    :: (Functor f, CanDesugarActions' tag)
    => Stage () (f tag) (f (Desugared' tag))
desugarStage = Stage
    { stageName = "desugaring"
    , stageConfig = ()
    , stageFunction = \() -> mapDesugarActions simpleActionMeaning
    }

-- | A stage that desugars actions within a functorial structure using a probabilistic interpretation.
--   It applies the 'probabilisticActionMeaning' to each action based on the provided
--   'ProbabilisticActionConfiguration'. This stage uses the 'NoOp' action embedding.
probabilisticDesugarStage
    :: (Functor f, CanDesugarActions' tag)
    => ProbabilisticActionConfiguration
    -> Stage ProbabilisticActionConfiguration (f tag) (f (Desugared' tag))
probabilisticDesugarStage pac = Stage
    { stageName = "probabilistic_desugaring"
    , stageConfig = pac
    , stageFunction = mapDesugarActions . probabilisticActionMeaning
    }
