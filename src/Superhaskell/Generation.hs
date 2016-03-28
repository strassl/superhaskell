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
  generated <- generate (gsGenState gs)
  let nBound = maximum $ map ((^._x) . rightBottom . eBox) generated
  let pruned = prune 0 es
  let nGenState = (gsGenState gs) { genBound = nBound }
  return gs{ entities = pruned ++ generated
           , gsGenState = nGenState }

generate :: RandomGen g => GenState -> Rand g [Entity]
generate _ = return mockScenery

prune :: VPBound -> [Entity] -> [Entity]
prune vpleft = filter (isLeftOfViewport vpleft)

mockScenery :: [Entity]
mockScenery = [ platform (V3 0 0 0) 3
              , platform (V3 4 0.5 0) 6
              , silverCoin (V3 6.5 6.5 0)
              , silverCoin (V3 7 7 1)
              , silverCoin (V3 5.5 5.5 (-2))
              , silverCoin (V3 7.5 7.5 2)
              , silverCoin (V3 6 6 (-1))
              , silverCoin (V3 5 5 (-3))
              ]

isLeftOfViewport :: VPBound -> Entity -> Bool
isLeftOfViewport leftBound _e@Entity{eBox = box} = boxRight <= leftBound
  where boxRight = (rightBottom box) ^._x

platform :: V3 Float -> Float -> Entity
platform pos length = Entity { eBox = Box pos (V2 length 0.75)
                             , eStyle = BoxStyle "ground_stone"
                             , eBehavior = NoopBehavior
                             , eCollisionGroup = SceneryCGroup}

silverCoin :: V3 Float -> Entity
silverCoin pos = Entity { eBox = Box pos (V2 1 1)
                        , eStyle = BoxStyle "silver_1"
                        , eBehavior = NoopBehavior
                        , eCollisionGroup = NilCGroup}
