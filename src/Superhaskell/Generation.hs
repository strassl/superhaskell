-- +------+-----------+-----+
--        |           |     |
--        |           |     |
--  prune |  viewport | gen |
--        |           |     |
--        |           |     |
-- +------+-----------+-----+
{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Generation (updateWorld) where

import Control.Monad.Random
import Control.Lens
import Linear.V2
import Linear.V3 (V3 (..))
import Superhaskell.Data.GameState
import Superhaskell.Data.Entity
import Superhaskell.Math

type VPBound = Float

updateWorld :: RandomGen g => GameState -> Rand g GameState
updateWorld gs@GameState{entities = es} = do
  generated <- generate
  let pruned = prune 0 es
  return gs{entities = pruned ++ generated}

generate :: RandomGen g => Rand g [Entity]
generate = return mockScenery

prune :: VPBound -> [Entity] -> [Entity]
prune vpleft = filter (isLeftOfViewport vpleft)

mockScenery :: [Entity]
mockScenery = [platform {eBox = Box (V3 0 0 0) (V2 3 0.75) }
              , platform {eBox = Box (V3 4 0.5 0) (V2 6 0.75) }
              , silverCoin {eBox = Box (V3 6.5 6.5 0) (V2 1 1) }
              , silverCoin {eBox = Box (V3 7 7 1) (V2 1 1) }
              , silverCoin {eBox = Box (V3 5.5 5.5 (-2)) (V2 1 1) }
              , silverCoin {eBox = Box (V3 7.5 7.5 2) (V2 1 1) }
              , silverCoin {eBox = Box (V3 6 6 (-1)) (V2 1 1) }
              , silverCoin {eBox = Box (V3 5 5 (-3)) (V2 1 1) }
              ]

isLeftOfViewport :: VPBound -> Entity -> Bool
isLeftOfViewport leftBound _e@Entity{eBox = box} = boxRight <= leftBound
  where boxRight = (rightBottom box) ^._x

platform :: Entity
platform = Entity {eBox = Box (V3 0 0 0) (V2 0 0)
                  , eStyle = BoxStyle "ground_stone"
                  , eBehavior = NoopBehavior
                  , eCollisionGroup = SceneryCGroup}

silverCoin :: Entity
silverCoin = Entity {eBox = Box (V3 0 0 0) (V2 0 0)
                    , eStyle = BoxStyle "silver_1"
                    , eBehavior = NoopBehavior
                    , eCollisionGroup = NilCGroup}
