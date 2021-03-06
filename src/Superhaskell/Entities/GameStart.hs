{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Entities.GameStart (
  gameStart, GameStart,
) where

import           Control.DeepSeq
import           GHC.Generics
import           Linear
import           Superhaskell.Data.Entities
import           Superhaskell.Data.GameState
import           Superhaskell.Data.InputState
import           Superhaskell.Data.RenderList
import           Superhaskell.Entities.Platform
import           Superhaskell.Entities.ScoreCounter
import           Superhaskell.Math


data GameStart = GameStart { _playerEntity :: Entity
                           } deriving (Show, Generic, NFData)

instance IsEntity GameStart where
  eBox _ = Box (V2 0 0) (V2 0 0)
  eRender gs _ _ = [KeyFrame [RenderSprite "game_over" (gsViewPort gs) 999] 1.0]
  eTick is gs i (GameStart p)
    | isRestart is = restartGame p i gs
    | otherwise = []

restartGame :: Entity -> Id -> GameState -> UpdateList
restartGame nextPlayer _ gs = [ ResetGame nextPlayer (Box (V2 0 0) (boxSize $ gsViewPort gs))
                              , Spawn counter
                              , Spawn initPlatform
                              ]
  where initPlatform = eWrap $ platform (boxAnchor (eBox nextPlayer) + V2 0 4) 6
        counter = eWrap $ scoreCounter (V2 0 0)

gameStart :: Entity -> GameStart
gameStart = GameStart
