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

data Box = Box { boxAnchor :: V3 Float
               , boxSize   :: V2 Float }
         deriving (Show, Generic, NFData)

leftTop :: Box -> V2 Float
leftTop _b@Box{boxAnchor = anchor} = withoutZ anchor

leftBottom :: Box -> V2 Float
leftBottom _b@Box{boxAnchor = anchor, boxSize = size} = over _y addHeight (withoutZ anchor)
  where addHeight = (+ (size ^._y))

rightBottom :: Box -> V2 Float
rightBottom _b@Box{boxAnchor = anchor, boxSize = size} = withoutZ anchor + size

rightTop :: Box -> V2 Float
rightTop _b@Box{boxAnchor = anchor, boxSize = size} = over _x addWidth (withoutZ anchor)
  where addWidth = (+ (size ^._x))

withoutZ :: V3 t -> V2 t
withoutZ = (^._xy)

moveBox :: V2 Float -> Box -> Box
moveBox (V2 x y) box@Box{boxAnchor=anchor} =
  box{boxAnchor = anchor ^+^ V3 x y 0}

boxContains :: V2 Float -> Box -> Bool
boxContains (V2 px py) Box{boxAnchor=(V3 bx by _), boxSize=(V2 sx sy)} =
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
pushOut Box{boxAnchor=V3 ax ay _, boxSize=V2 aw ah}
        b@Box{boxAnchor=V3 bx by _, boxSize=V2 bw bh} =
  let dists = [ (V2 (ax - (bx + bw)) 0,                RightEdge)
              , (V2 (ax + aw - bx)   0,                LeftEdge)
              , (V2 0                (ay - (by + bh)), BottomEdge)
              , (V2 0                (ay + ah - by),   TopEdge) ]
      minDist = minimumBy (comparing (norm . fst)) dists
  in (moveBox (fst minDist + signum (fst minDist) * V2 eps eps) b, snd minDist)
