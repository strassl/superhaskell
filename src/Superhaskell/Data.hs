module Superhaskell.Data (
  GameState(..)
) where

data GameState = GameState { running :: Bool } deriving Show
