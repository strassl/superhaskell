module Superhaskell.Data (
    GameState(..)
  , InputState(..)
) where

data GameState = GameState { running :: Bool } deriving Show

data InputState = InputState { wantQuit :: Bool} deriving Show
