{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Entities.Player (
  player, Player, playerManualSpeed, playerJumpTime, playerAcceleration,
  playerStartSpeed
) where

import           Control.DeepSeq
import           GHC.Generics
import           Linear
import           Superhaskell.Data.Entities
import           Superhaskell.Data.GameState
import           Superhaskell.Data.InputState
import           Superhaskell.Data.RenderList
import           Superhaskell.Math
import           Superhaskell.Processing
import           Superhaskell.Entities.GameStart

-- Controlled speed in units/tick.
playerManualSpeed :: Float
playerManualSpeed = 3 / tps

-- Number of ticks the player needs to reach the apex of a jump.
playerJumpTime :: Float
playerJumpTime = 0.5 * tps

-- Acceleration of the player in units/tick².
playerAcceleration :: Float
playerAcceleration = 0.1 / tps / tps

-- Deacceleration of boosts in units/tick².
playerDeacceleration :: Float
playerDeacceleration = 15 / tps / tps

-- Start speed in units/tick.
playerStartSpeed :: Float
playerStartSpeed = 1 / tps

-- Drop speed in units/tick.
playerDropSpeed :: Float
playerDropSpeed = 20 / tps

-- Boost speed of the player in units/tick.
playerBoostSpeed :: Float
playerBoostSpeed = 10 / tps

-- Number of ticks after which the placer is game over.
playerGameOver :: Float
playerGameOver = 2 * tps

-- Not exported -> no need to scope :)
data Player = Player { pos        :: V2 Float
                     , speed      :: Float
                     , extraSpeed :: Float
                     , state      :: PlayerState
                     }
            deriving (Show, Generic, NFData)

data PlayerState = InAir PlayerInAir | Dropping Float | OnGround
                 deriving (Show, Generic, NFData)

data PlayerInAir = PlayerInAir { fallingTicks :: Float
                               , canBoost     :: Bool
                               }
                 deriving (Show, Generic, NFData)

instance IsEntity Player where
  eCollisionGroup _ = PlayerCGroup

  eBox p = Box (pos p) (V2 0.72463768115942028986 1)

  eRender _ _ p = case state p of
                    InAir _ -> [kf_jump]
                    Dropping _ -> [kf_jump]
                    OnGround -> [kf_walk1, kf_walk2]
    where kf_walk1 = KeyFrame [RenderSprite "bunny1_walk1" (eBox p) 0] 0.2
          kf_walk2 = KeyFrame [RenderSprite "bunny1_walk2" (eBox p) 0] 0.2
          kf_jump = KeyFrame [RenderSprite "bunny1_jump" (eBox p) 0] 1

  eTick = simpleTick $ \is gs p ->
    let p' = case state p of
               InAir _    -> tickInAir is gs p
               Dropping _ -> tickDropping is gs p
               OnGround   -> tickOnGround is gs p
        speed' = speed p' + playerAcceleration
        extraSpeed' = max 0 (extraSpeed p' - playerDeacceleration)
        p'' = p'{speed=speed', extraSpeed=extraSpeed'}
    in if testGameOver p''
      then eWrap $ gameStart (eWrap player)
      else eWrap p''
        
  eCollide _ other gs _ p =
    case eCollisionGroup other of
      SceneryCGroup -> (gs, collideWithScenery other p)
      _ -> (gs, p)

tickInAir :: InputState -> GameState -> Player -> Player
tickInAir is gs p@Player{pos=pos, speed=speed, extraSpeed=extraSpeed, state=InAir state} =
  let PlayerInAir{fallingTicks=fallingTicks, canBoost=canBoost} = state
      (V2 inputX _) = isDirection is

      gravityDeltaPos = V2 0 (gravity * fallingTicks)
      moveDeltaPos = V2 (max 0 (inputX * playerManualSpeed)) 0
      baseDeltaPos = V2 (speed + extraSpeed) 0
      pos' = pos ^+^ gravityDeltaPos ^+^ moveDeltaPos ^+^ baseDeltaPos

      doBoost = canBoost && isBoost is
      fallingTicks' = if fallingTicks < -7 && not (isJump is)
                        then -7
                        else fallingTicks + 1
      state'
        | isDrop is = Dropping 0
        | doBoost   = InAir state{ fallingTicks=fallingTicks'
                                 , canBoost=False
                                 }
        | otherwise = InAir state{fallingTicks=fallingTicks'}
      extraSpeed' = if doBoost
                      then extraSpeed + playerBoostSpeed
                      else extraSpeed

  in p{pos=pos', state=state', extraSpeed=extraSpeed'}
tickInAir _ _ _ =
  error "Player is not InAir"

tickDropping :: InputState -> GameState -> Player -> Player
tickDropping _ gs p@Player{pos=pos, state=Dropping droppingTicks} =
  let droppingTicks' = droppingTicks + 1
  in p{ pos=pos + V2 0 playerDropSpeed
      , state=Dropping droppingTicks' }
tickDropping _ _ _ =
  error "Player not Dropping"

testGameOver :: Player -> Bool
testGameOver Player{state=(Dropping droppingTicks)} = droppingTicks > playerGameOver
testGameOver Player{state=(InAir (PlayerInAir fallingTicks _))} = fallingTicks > playerGameOver
testGameOver _ = False

tickOnGround :: InputState -> GameState -> Player -> Player
tickOnGround is gs p@Player{pos=pos, speed=speed, extraSpeed=extraSpeed, state=OnGround} =
  let (V2 inputX _) = isDirection is
      box = eBox p

      moveDeltaPos = V2 (max 0 (inputX * playerManualSpeed)) 0
      baseDeltaPos = V2 (speed + extraSpeed) 0
      pos' = pos ^+^ moveDeltaPos ^+^ baseDeltaPos

      offEdge =  null (entitiesAtInGroup (leftBottom box + V2 0 (2 * eps)) SceneryCGroup gs)
              && null (entitiesAtInGroup (rightBottom box + V2 0 (2 * eps)) SceneryCGroup gs)

      state'
        | isJump is = InAir (PlayerInAir (-playerJumpTime) True)
        | offEdge   = InAir (PlayerInAir 1 True)
        | otherwise = OnGround
  in p{pos=pos', state=state'}
tickOnGround _ _ _ =
  error "Player not OnGround"

collideWithScenery :: IsEntity o => o -> Player -> Player
collideWithScenery other p =
  let (Box{boxAnchor=V2 x' y'}, edge) = pushOut (eBox other) (eBox p)
  in p{pos=V2 x' y', state=if edge == BottomEdge then OnGround else state p}

player :: Player
player = Player (V2 4 2) playerStartSpeed 0 (Dropping 0)
