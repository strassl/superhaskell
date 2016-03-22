module Superhaskell.Data.GameState (
    GameState(..), initialGameState
) where

import           Linear.V2                (V2 (..))
import           Linear.V3                (V3 (..))
import           Superhaskell.Data.Entity
import           Superhaskell.Math

data GameState = GameState { entities :: [Entity]
                           , running  :: Bool } deriving Show

initialGameState :: GameState
initialGameState = GameState { running = True
                             , entities = mockEntities }

mockEntities :: [Entity]
mockEntities = [ platform { box = Box (V3 0 0 0) (V2 0.5 0.5) }
               , platform { box = Box (V3 1 0.5 0) (V2 0.5 0.8) }
               ]

platform :: Entity
platform = Entity { box = Box (V3 0 0 0) (V2 0 0), style = BoxStyle, behavior = NoopBehavior }
