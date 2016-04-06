{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Entities.Player (
  player, Player, playerManualSpeed, playerJumpTime, playerAcceleration,
  playerStartSpeed
) where

import           Control.Applicative
import           Control.DeepSeq
import           GHC.Generics
import           Linear
import           Superhaskell.Data.GameState
import           Superhaskell.Data.InputState
import           Superhaskell.Data.RenderList
import           Superhaskell.Math
import           Superhaskell.Processing

-- Controlled speed in units/tick.
playerManualSpeed :: Float
playerManualSpeed = 3 / tps

-- Number of ticks the player needs to reach the apex of a jump.
playerJumpTime :: Float
playerJumpTime = 0.5 * tps

-- Acceleration of the player in units/tick².
playerAcceleration :: Float
playerAcceleration = 0.1 / tps / tps

-- Start speed in units/tick.
playerStartSpeed :: Float
playerStartSpeed = 1 / tps

-- Not exported -> no need to scope :)
data Player = Player { pos     :: V2 Float
                     , falling :: Maybe Float
                     , speed   :: Float
                     }
            deriving (Show, Generic, NFData)

instance IsEntity Player where
  eCollisionGroup _ = PlayerCGroup

  eBox p = Box (pos p) (V2 0.5970149253731343 1)

  eRender _ _ p = [RenderSprite "bunny1_stand" (eBox p) 0]

  eTick is gs _ p@Player{pos=pos, falling=falling, speed=speed} =
    let (V2 inputX _) = isDirection is
        box = eBox p

        (gravityFalling, gravityDeltaPos) = applyGravity falling
        moveDeltaPos = V2 (inputX * playerManualSpeed) 0
        baseDeltaPos = V2 speed 0
        pos' = pos ^+^ gravityDeltaPos ^+^ moveDeltaPos ^+^ baseDeltaPos

        offEdge =  null (entitiesAtInGroup (leftBottom box + V2 0 (2 * eps)) SceneryCGroup gs)
                && null (entitiesAtInGroup (rightBottom box + V2 0 (2 * eps)) SceneryCGroup gs)
        jump = if isJump is then Just (-playerJumpTime) else Nothing
        falling' = gravityFalling <|> jump <|> if offEdge then Just 1 else Nothing

        speed' = speed + playerAcceleration
    in (gs, p{pos=pos', falling=falling', speed=speed'})

  eCollide _ other gs _ p =
    case eCollisionGroup other of
      SceneryCGroup -> (gs, collideWithScenery other p)
      _ -> (gs, p)

collideWithScenery :: IsEntity o => o -> Player -> Player
collideWithScenery other p =
  let (Box{boxAnchor=V2 x' y'}, edge) = pushOut (eBox other) (eBox p)
  in p{pos=V2 x' y', falling=if edge == BottomEdge then Nothing else falling p}

player :: Player
player = Player (V2 4 2) (Just 0) playerStartSpeed

applyGravity :: Maybe Float -> (Maybe Float, V2 Float)
applyGravity (Just time) = (Just (time + 1), V2 0 (gravity * time))
applyGravity Nothing = (Nothing, V2 0 0)
