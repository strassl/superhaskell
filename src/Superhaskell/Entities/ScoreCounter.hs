{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Entities.ScoreCounter (ScoreCounter, scoreCounter) where

import           Control.DeepSeq
import           Data.Text (Text, snoc)
import           GHC.Generics
import           Linear
import           Superhaskell.Data.Entities
import           Superhaskell.Data.GameState
import           Superhaskell.Data.RenderList
import           Superhaskell.Math

data ScoreCounter = ScoreCounter (V2 Float) deriving (Show, Generic, NFData)

instance IsEntity ScoreCounter where
  eBox (ScoreCounter _ ) = Box (V2 0 0) (V2 0 0)
  eRender gs _ (ScoreCounter offset) = [KeyFrame (scoreToRenderList offsetAnchor (round score)) 1]
    where score = left $ eBox player
          player = esPlayer $ gsEntities gs
          offsetAnchor = boxAnchor (gsViewPort gs) + offset

scoreCounter :: V2 Float -> ScoreCounter
scoreCounter = ScoreCounter

scoreToRenderList :: V2 Float -> Int -> RenderList
scoreToRenderList anchor i = zipWith (\b t -> RenderSprite t b zIndex) boxes textures
    where textures = map digitToTexture digits
          boxes = map (\(n, _) -> Box (digitAnchor n) (V2 digitWidth digitHeight)) (zip [0..] digits)
          digits = show i
          digitAnchor n = anchor + V2 (n*digitWidth) 0

digitToTexture :: Char -> Text
digitToTexture num = "font_" `snoc` num

digitWidth :: Float
digitWidth = 1

digitHeight :: Float
digitHeight = 1

zIndex :: Float
zIndex = 99
