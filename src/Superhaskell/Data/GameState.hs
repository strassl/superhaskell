{-# LANGUAGE OverloadedStrings #-}
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
mockEntities = [ platform { box = Box (V3 0 0 0) (V2 3 0.75) }
               , platform { box = Box (V3 4 0.5 0) (V2 6 0.75) }
               , silverCoin { box = Box (V3 6.5 6.5 0) (V2 1 1) }
               , silverCoin { box = Box (V3 7 7 1) (V2 1 1) }
               , silverCoin { box = Box (V3 5.5 5.5 (-2)) (V2 1 1) }
               , silverCoin { box = Box (V3 7.5 7.5 2) (V2 1 1) }
               , silverCoin { box = Box (V3 6 6 (-1)) (V2 1 1) }
               , silverCoin { box = Box (V3 5 5 (-3)) (V2 1 1) }
               ]

platform :: Entity
platform = Entity { box = Box (V3 0 0 0) (V2 0 0), style = BoxStyle "ground_stone", behavior = NoopBehavior }

silverCoin :: Entity
silverCoin = Entity { box = Box (V3 0 0 0) (V2 0 0), style = BoxStyle "silver_1", behavior = NoopBehavior }
