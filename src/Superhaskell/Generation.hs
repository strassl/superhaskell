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

updateWorld :: RandomGen g => GameState -> Rand g GameState
updateWorld gs@GameState{entities = es, gsViewPort = vp} = do
  generated <- generate vp (gsGenState gs)
  let nBound = maximum $ genBound (gsGenState gs):map ((^._x) . rightBottom . eBox) generated
  let pruned = prune 0 es -- TODO needs an actual left bound
  let nGenState = (gsGenState gs) { genBound = nBound }
  let nes = pruned ++ generated
  return gs{ entities = nes
           , gsGenState = nGenState }

prune :: Float -> [Entity] -> [Entity]
prune vpleft = filter (not . isLeftOfViewport vpleft)

-- TODO This whole function needs a real implementation
generate :: RandomGen g => V2 Float -> GenState -> Rand g [Entity]
generate _vp@(V2 w h) _gs@GenState{genBound = bound}
  | bound >= w = return []
  | otherwise = do
    pos_x <- getRandomR (bound, w) -- TODO Actually not 100% correct because it ignores width - recheck this
    pos_y <- getRandomR (1, h-1)
    let p = platform (V3 pos_x pos_y 0) 3
    let c = silverCoin (V3 (pos_x+1.5) (pos_y - 1.5) 1)
    return [p, c]

isLeftOfViewport :: Float -> Entity -> Bool
isLeftOfViewport leftBound _e@Entity{eBox = box} = boxRight <= leftBound
  where boxRight = rightBottom box ^._x

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