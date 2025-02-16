module BellKAT.Utils.Automata.Execution.Common
    ( ExecutionParams(..)
    , ExecutionError(..)
    ) where

import           Data.Default

data ExecutionParams s = EP 
    { maxOptionsPerState :: Maybe Int
    , isValidState :: s -> Bool
    }

data ExecutionError = TooManyStates | InvalidStateReached

instance Default (ExecutionParams s) where
    def = EP Nothing (const True)
