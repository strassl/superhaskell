-- +------+-----------+-----+
--        |           |     |
--        |           |     |
--  prune |  viewport | gen |
--        |           |     |
--        |           |     |
-- +------+-----------+-----+
{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Generation (updateWorld) where

import           Control.Lens
import           Control.Monad.Random
import           Data.Foldable
import           Data.Function                  (on)
import           Linear.V2
import           Superhaskell.Data.Entities
import           Superhaskell.Data.GameState
import           Superhaskell.Entities.Platform
import           Superhaskell.Math
import Superhaskell.Processing
import Superhaskell.Entities.Cloud

newtype T4 a b c d = T4 (a, b, c, d)
instance (Random a, Random b, Random c, Random d) => Random (T4 a b c d) where
  randomR (T4 (a, b, c, d), T4 (a', b', c', d')) g =
    let (a'', g') = randomR (a, a') g
        (b'', g'') = randomR (b, b') g'
        (c'', g''') = randomR (c, c') g''
        (d'', g'''') = randomR (d, d') g'''
    in (T4 (a'', b'', c'', d''), g'''')
  random g =
    let (a, g') = random g
        (b, g'') = random g'
        (c, g''') = random g''
        (d, g'''') = random g'''
    in (T4 (a, b, c, d), g'''')

cloudsPerTick :: Float
cloudsPerTick = 1 / (tps * 3)

updateWorld :: RandomGen g => GameState -> Rand g GameState
updateWorld gs = updateLevel gs >>= updateBackground

updateBackground :: RandomGen g => GameState -> Rand g GameState
updateBackground = generateClouds

generateClouds :: RandomGen g => GameState -> Rand g GameState
generateClouds gs =
  let player = esPlayer $ gsEntities gs
      (Box (V2 _ py) _) = eBox player
      vph = viewPort ^. _y + 1
  in getRandomR (T4 (0, py - vph / 2, 1.9, 0.75),
                 T4 (1, py + vph / 2, 2.25, 1))
     >>= \(T4 (chance, y, w, h)) ->
       return $ if chance < cloudsPerTick
         then gs{gsEntities=insertOther (eWrap $ cloud (V2 (viewPort^._x) y) (V2 w h) 0) (gsEntities gs)}
         else gs

updateLevel :: RandomGen g => GameState -> Rand g GameState
updateLevel gs@GameState{gsEntities = es, gsViewPort = vp@(Box (V2 vpl _) _)} = do
  -- TODO handle player here
  let lastPlatformY = (^._y) $ maximumBy (compare `on` (^._x)) $ fmap (rightTop . eBox) (gsEntities gs)
  generated <- generate vp lastPlatformY (gsGenState gs)
  let nBound = maximum $ genBound (gsGenState gs):map ((^._x) . rightBottom . eBox) generated
  let pruned = prune vpl es
  let nGenState = (gsGenState gs) { genBound = nBound }
  let nes = appendOthers generated pruned
  return gs{ gsEntities = nes
           , gsGenState = nGenState }

prune :: Float -> Entities -> Entities
prune vpleft = filterOthers shouldKeep
    where shouldKeep e = not (isLeftOfViewport vpleft e) || eCollisionGroup e `notElem` prunableGroups

-- TODO correct height
-- We partition the world (horizontally) into partitionWidth wide sections (at least 1)
-- In each partition we generate a single platform
generate :: RandomGen g => Box -> Float -> GenState -> Rand g [Entity]
generate vp lastY _gs@GenState{genBound = bound}
  | bound >= (right vp + genAhead) = return []
  | otherwise = do
    let parts = partition bound (right vp + genAhead)
    mapM (\p -> eWrap <$> generatePlatform (lastY-genUpRange, lastY+genDownRange) p) parts

partition :: Float -> Float -> [(Float, Float)]
partition l r = zip parts (tail' parts)
  where w = r - l
        pc = floor(w / partitionWidth)
        parts = scanl (+) l (replicate pc partitionWidth)

generatePlatform :: RandomGen g => (Float, Float) -> (Float, Float) -> Rand g Platform
generatePlatform (b, t) (l, r)= do
    pos_x <- getRandomR (l+borderWidth, r-platformWidth-borderWidth)
    pos_y <- getRandomR (b, t)
    let p = platform (V2 pos_x pos_y) platformWidth
    return p

isLeftOfViewport :: IsEntity e => Float -> e -> Bool
isLeftOfViewport leftBound e = boxRight <= leftBound
  where boxRight = rightBottom (eBox e) ^._x

partitionWidth :: Float
partitionWidth = 7

-- How much of the partition should be kept free of platforms
-- Constrains anchor on each side
borderWidth :: Float
borderWidth = 2

platformWidth :: Float
platformWidth = 4

genAhead :: Float
genAhead = 8

genUpRange :: Float
genUpRange = 4

genDownRange :: Float
genDownRange = 2

prunableGroups :: [CollisionGroup]
prunableGroups = [SceneryCGroup]

tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs
