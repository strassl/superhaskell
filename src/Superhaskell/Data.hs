module Superhaskell.Data (
    GameState(..), initialGameState
  , InputState(..), defaultInputState
) where

data GameState = GameState { running :: Bool } deriving Show

initialGameState :: GameState
initialGameState = GameState { running = True }

data InputState = InputState { wantQuit :: Bool} deriving Show

defaultInputState :: InputState
defaultInputState = InputState False
