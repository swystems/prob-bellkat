{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module BellKAT.Bundles.Core where

import Control.Monad.Logger
import Data.Text (Text)

data Stage c a b = Stage { stageName :: Text, stageConfig :: c, stageFunction :: c -> a -> b }

data Pipeline a b where
    PLeaf :: Show a => Pipeline a a 
    PStage :: forall a b c d. (Show a, Show c) => Stage c a d -> Pipeline d b -> Pipeline a b

stage :: (Show a, Show c, Show b) => Stage c a b -> Pipeline a b
stage x = PStage x PLeaf

(>>>) :: Pipeline a b -> Pipeline b c -> Pipeline a c
PLeaf >>> x = x
PStage a p >>> x = PStage a (p >>> x)

executePipeline :: MonadLogger m => Pipeline a b -> a -> m b
executePipeline PLeaf x = do
    $(logInfo) "Pipeline finished. Final state:"
    $(logInfoSH) x
    pure x
executePipeline (PStage (Stage { stageName = name, stageConfig = c, stageFunction = f }) next) input = do
    $(logInfo) ("Executing stage: " <> name)
    $(logInfoSH) input
    $(logInfoSH) c
    executePipeline next (f c input)

runLoggedPipeline :: Pipeline a b -> a -> IO b
runLoggedPipeline p input = runStderrLoggingT $ executePipeline p input
