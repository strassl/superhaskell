{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Superhaskell.Math (
    eps
  , Edge(..)
  , Box(..)
  , leftTop, leftBottom, rightBottom, rightTop
  , moveBox, pushOut
  , boxContains, boxOverlaps
) where

import           Control.DeepSeq
import           Control.Lens
import           Data.List
import           Data.Ord
import           GHC.Generics
import           Linear

-- A small number.
eps :: Float
eps = 1 / 1024

data Edge = LeftEdge | TopEdge | RightEdge | BottomEdge
          deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, NFData)

data Box = Box { boxAnchor :: V2 Float
               , boxSize   :: V2 Float }
         deriving (Show, Generic, NFData)

leftTop :: Box -> V2 Float
leftTop (Box xy _) = xy

leftBottom :: Box -> V2 Float
leftBottom (Box (V2 x y) (V2 _ h)) = V2 x (y + h)

rightBottom :: Box -> V2 Float
rightBottom (Box xy wh) = xy ^+^ wh

rightTop :: Box -> V2 Float
rightTop (Box (V2 x y) (V2 w _)) = V2 (x + w) y

moveBox :: V2 Float -> Box -> Box
moveBox offset box@Box{boxAnchor=anchor} =
  box{boxAnchor = anchor ^+^ offset}

boxContains :: V2 Float -> Box -> Bool
boxContains (V2 px py) (Box (V2 bx by) (V2 sx sy)) =
     bx < px && px < bx + sx
  && by < py && py < by + sy

-- TODO optimize this!?!?!
boxOverlaps :: Box -> Box -> Bool
boxOverlaps a b = anyCorner a b || anyCorner b a
  where
    anyCorner a b =
      any (flip boxContains a . ($ b)) [leftTop, leftBottom, rightBottom, rightTop]

-- | When a and b overlap, b will be pushed out of a using the shortest possible
-- distance. If a and b do not overlap... it does strange things, I guess.
-- It returns the edge of the *second* box that has the contact now.
pushOut :: Box -> Box -> (Box, Edge)
pushOut (Box (V2 ax ay) (V2 aw ah)) b@(Box (V2 bx by) (V2 bw bh)) =
  let dists = [ (V2 (ax - (bx + bw)) 0,                RightEdge)
              , (V2 (ax + aw - bx)   0,                LeftEdge)
              , (V2 0                (ay - (by + bh)), BottomEdge)
              , (V2 0                (ay + ah - by),   TopEdge) ]
      minDist = minimumBy (comparing (norm . fst)) dists
  in (moveBox (fst minDist + signum (fst minDist) * V2 eps eps) b, snd minDist)
