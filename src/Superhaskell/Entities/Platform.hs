{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Entities.Platform (Platform, platform) where

import           Control.DeepSeq
import           GHC.Generics
import           Linear
import           Superhaskell.Data.GameState
import           Superhaskell.Data.RenderList
import           Superhaskell.Math

data Platform = Platform Box deriving (Show, Generic, NFData)

instance IsEntity Platform where
  eBox (Platform box) = box
  eRender _ (Platform box) = [RenderSprite "ground_stone" box]
  eCollisionGroup _ = SceneryCGroup

platform :: V3 Float -> Float -> Platform
platform pos length = Platform (Box pos (V2 length 0.75))