{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Superhaskell.Data.InputState (
    InputState(..), defaultInputState
) where

import           Control.DeepSeq
import           GHC.Generics
import           Linear

-- | Input state.
data InputState = InputState { -- | True if the user wants to quit the game.
                               isWantQuit  :: Bool
                               -- | A vector describing the input direction.
                               -- Is of length 0 to 1.
                             , isDirection :: V2 Float
                             }
                deriving (Show, Generic, NFData)

defaultInputState :: InputState
defaultInputState = InputState False (V2 0 0)
