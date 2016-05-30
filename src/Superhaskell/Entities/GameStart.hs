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
import           Superhaskell.Math
import           Superhaskell.Entities.ScoreCounter
import           Superhaskell.Entities.Platform


data GameStart = GameStart { playerEntity :: Entity
                           } deriving (Show, Generic, NFData)

instance IsEntity GameStart where
  eBox _ = Box (V2 0 0) (V2 0 0)
  eCollisionGroup _ = NilCGroup
  eRender gs i e = [KeyFrame [RenderSprite "game_start" (gsViewPort gs) 999] 1.0]
  eTick is gs i e@(GameStart p)
    | isRestart is = restartGame p i gs
    | otherwise = gs

restartGame :: Entity -> Id -> GameState -> GameState
restartGame nextPlayer i gs = gs { gsEntities = ents
                                 , gsGenState = initialGenState
                                 , gsViewPort = Box (V2 0 0) (boxSize $ gsViewPort gs)
                                 }
  where initPlatform = eWrap $ platform (boxAnchor (eBox nextPlayer) + V2 0 4) 6
        counter = eWrap $ scoreCounter (V2 0 0)
        ents = appendOthers [counter, initPlatform] $ makeEntities nextPlayer

gameStart :: Entity -> GameStart
gameStart = GameStart
