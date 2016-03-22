module Superhaskell.Data.GameState (
    GameState(..), initialGameState
) where

data GameState = GameState { running :: Bool } deriving Show

initialGameState :: GameState
initialGameState = GameState { running = True }
