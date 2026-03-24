{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module BellKAT.Bundles.Desugaring
    ( desugarStage
    , probabilisticDesugarStage
    , probabilisticOpDesugarStage
    ) where

import           Data.Default (Default)
import           Data.Proxy (Proxy)

import           BellKAT.ActionEmbeddings
import           BellKAT.Bundles.Core
import           BellKAT.Definitions.Core (Op)

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

probabilisticOpDesugarStage
    :: (Functor f, Default rTag, CanDesugarActions (Op rTag) a)
    => Proxy rTag
    -> ProbabilisticActionConfiguration
    -> Stage ProbabilisticActionConfiguration (f a) (f (Desugared (Op rTag) a))
probabilisticOpDesugarStage (_ :: Proxy rTag) pac = Stage
    { stageName = "probabilistic_op_desugaring"
    , stageConfig = pac
    , stageFunction = mapDesugarActions @(Op rTag) . probabilisticOpActionMeaning
    }
