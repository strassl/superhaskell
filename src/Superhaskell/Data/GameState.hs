module Superhaskell.Data.GameState (
    GameState(..), initialGameState
) where

import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Superhaskell.Data.Entity

data GameState = GameState { entities :: [Entity]
                           , running :: Bool } deriving Show

initialGameState :: GameState
initialGameState = GameState { running = True
                             , entities = mockEntities }

mockEntities :: [Entity]
mockEntities = [ platform { box = Box (V3 0 0 0) (V2 50 50) }
               , platform { box = Box (V3 100 50 0) (V2 50 80) }
               ]

platform :: Entity
platform = Entity { box = Box (V3 0 0 0) (V2 0 0), style = BoxStyle, behavior = NoopBehavior }
