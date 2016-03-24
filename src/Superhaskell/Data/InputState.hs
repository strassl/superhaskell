{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Superhaskell.Data.InputState (
    InputState(..), defaultInputState
) where

import           Control.DeepSeq
import           GHC.Generics

data InputState = InputState { wantQuit :: Bool }
                deriving (Show, Generic, NFData)

defaultInputState :: InputState
defaultInputState = InputState False
