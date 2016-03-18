module Superhaskell.Processing (tickGameState) where

import Superhaskell.Data

tickGameState :: InputState -> GameState -> GameState
tickGameState = const id
