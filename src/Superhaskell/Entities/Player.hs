{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Entities.Player (player, Player) where

import           Control.DeepSeq
import           GHC.Generics
import           Linear
import           Superhaskell.Data.GameState
import           Superhaskell.Data.RenderList
import           Superhaskell.Math

-- Not exported -> no need to scope :)
data Player = Player { pos :: V2 Float
                     , falling :: Maybe Float
                     }
            deriving (Show, Generic, NFData)

instance IsEntity Player where
  eCollisionGroup _ = PlayerCGroup

  eBox Player{pos=V2 x y} = Box (V3 x y 0) (V2 0.5970149253731343 1)

  eRender _ p = [RenderSprite "bunny1_stand" (eBox p)]

player :: Player
player = Player (V2 4 2) (Just 0)
