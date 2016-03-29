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
import Superhaskell.Data.Entities
import Superhaskell.Math

updateWorld :: RandomGen g => GameState -> Rand g GameState
updateWorld gs@GameState{gsEntities = es, gsViewPort = vp@(Box (V3 vpl _ _) _)} = do
  generated <- generate vp (gsGenState gs)
  let nBound = maximum $ genBound (gsGenState gs):map ((^._x) . rightBottom . eBox) generated
  let pruned = prune vpl es
  let nGenState = (gsGenState gs) { genBound = nBound }
  let nes = pruned `appendOthers` generated
  return gs{ gsEntities = nes
           , gsGenState = nGenState }

prune :: Float -> Entities -> Entities
prune vpleft = filterOthers (not . isLeftOfViewport vpleft)

-- TODO correct height
-- We partition the world (horizontally) into partitionWidth wide sections (at least 1)
-- In each partition we generate a single platform
generate :: RandomGen g => Box-> GenState -> Rand g [Entity]
generate _vp@(Box (V3 l t _) (V2 w h)) _gs@GenState{genBound = bound}
  | bound >= (l+w+genAhead) = return []
  | otherwise = do
    let parts = partition bound (l+w+genAhead)
    mapM (generatePlatform (t+h/4, t+h*3/4)) parts

partition :: Float -> Float -> [(Float, Float)]
partition l r = zip parts (tail' parts)
  where w = r - l
        pc = floor(w / partitionWidth)
        parts = scanl (+) l (replicate pc partitionWidth)

generatePlatform :: RandomGen g => (Float, Float) -> (Float, Float) -> Rand g Entity
generatePlatform (b, t) (l, r)= do
    pos_x <- getRandomR (l, r) -- TODO Actually not 100% correct because it ignores width - recheck this
    pos_y <- getRandomR (b, t)
    let p = platform (V3 pos_x pos_y 0) 3
    return p

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

partitionWidth :: Float
partitionWidth = 4

genAhead :: Float
genAhead = 8


tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs
