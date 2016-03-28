{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Generation (updateWorld) where

import Superhaskell.Data.GameState
import Superhaskell.Data.Entity
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Superhaskell.Math

updateWorld :: GameState -> GameState
updateWorld gs = gs { entities = nes }
  where es = entities gs
        nes = mockScenery

mockScenery :: [Entity]
mockScenery = [ platform { eBox = Box (V3 0 0 0) (V2 3 0.75) }
              , platform { eBox = Box (V3 4 0.5 0) (V2 6 0.75) }
              , silverCoin { eBox = Box (V3 6.5 6.5 0) (V2 1 1) }
              , silverCoin { eBox = Box (V3 7 7 1) (V2 1 1) }
              , silverCoin { eBox = Box (V3 5.5 5.5 (-2)) (V2 1 1) }
              , silverCoin { eBox = Box (V3 7.5 7.5 2) (V2 1 1) }
              , silverCoin { eBox = Box (V3 6 6 (-1)) (V2 1 1) }
              , silverCoin { eBox = Box (V3 5 5 (-3)) (V2 1 1) }
              ]


platform :: Entity
platform = Entity { eBox = Box (V3 0 0 0) (V2 0 0)
                  , eStyle = BoxStyle "ground_stone"
                  , eBehavior = NoopBehavior
                  , eCollisionGroup = SceneryCGroup }

silverCoin :: Entity
silverCoin = Entity { eBox = Box (V3 0 0 0) (V2 0 0)
                    , eStyle = BoxStyle "silver_1"
                    , eBehavior = NoopBehavior
                    , eCollisionGroup = NilCGroup }
