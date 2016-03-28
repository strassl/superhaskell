{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Superhaskell.Math (
    Box(..)
  , leftTop
  , rightBottom
) where

import           Control.Lens
import           Control.DeepSeq
import           GHC.Generics
import           Linear

data Box = Box { boxAnchor :: V3 Float
               , boxSize   :: V2 Float }
         deriving (Show, Generic, NFData)

leftTop :: Box -> V2 Float
leftTop _b@Box{boxAnchor = anchor} = withoutZ anchor

leftBottom :: Box -> V2 Float
leftBottom _b@Box{boxAnchor = anchor, boxSize = size} = over _y addHeight (withoutZ anchor)
  where addHeight = (+ (size ^._y))

rightBottom :: Box -> V2 Float
rightBottom _b@Box{boxAnchor = anchor, boxSize = size} = (withoutZ anchor) + size

rightTop :: Box -> V2 Float
rightTop _b@Box{boxAnchor = anchor, boxSize = size} = over _x addWidth (withoutZ anchor)
  where addWidth = (+ (size ^._x))

withoutZ :: V3 t -> V2 t
withoutZ = (^._xy)
