{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Entities.Cloud (
  cloud, Cloud
) where

import           Control.DeepSeq
import           Control.Lens
import           GHC.Generics
import           Linear                       hiding (distance)
import           Superhaskell.Data.GameState
import           Superhaskell.Data.RenderList
import           Superhaskell.Math
import           Superhaskell.Processing

-- Base cloud speed in units/tick.
cloudSpeed :: Float
cloudSpeed = 3 / tps

data Cloud = Cloud { pos      :: V2 Float
                   , size     :: V2 Float
                   , distance :: Float }
           deriving (Show, Generic, NFData)

instance IsEntity Cloud where
  eBox c = Box (pos c) (size c)

  eRender gs _ c = [KeyFrame [sp] 1]
    where offset = V2 (boxAnchor (gsViewPort gs) ^. _x) 0
          sp = RenderSprite "cloud"
                            (moveBox offset (eBox c))
                            (-100 - distance c)

  eTick = simpleTick tick
    where tick _ _ c = c{pos=pos c ^-^ V2 cloudSpeed 0}

cloud :: V2 Float -> V2 Float -> Float -> Cloud
cloud = Cloud
