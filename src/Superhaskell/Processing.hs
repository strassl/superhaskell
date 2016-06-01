module Superhaskell.Processing (tickGameState, gravity, tps, viewPort) where

import           Control.Lens
import           Data.Foldable
import           Linear.V2
import           Superhaskell.Data.Entities
import           Superhaskell.Data.GameState
import           Superhaskell.Data.InputState
import           Superhaskell.Math

-- Ticks per second.
tps :: Float
tps = 144

-- Gravity in units/tick².
gravity :: Float
gravity = 30 / tps / tps

-- Size of the viewport.
viewPort :: V2 Float
viewPort = V2 16 9 * 1.25

-- Advances the game state by one tick.
tickGameState :: InputState -> GameState -> GameState
tickGameState is gs
  | isWantQuit is = gs{ gsRunning = False }
  | otherwise     = (moveViewPort . collideEntities . tickEntities is) gs

moveViewPort :: GameState -> GameState
moveViewPort gs@GameState{gsViewPort = vp@(Box _ wh)} = gs{gsViewPort = withCenter newCamCenter vp}
  where playerCenter = center $ eBox $ esPlayer $ gsEntities gs
        targetCenter = over _x (\v -> v + ((wh/3) ^._x)) playerCenter
        targetWeight = 0.1 -- Decrease to make it more rubber-bandy, never set to 0
        oldCamCenter = center vp
        newCamCenter = (oldCamCenter * (1-targetWeight)) + (targetCenter * targetWeight)

tickEntities :: InputState -> GameState -> GameState
tickEntities is startGs =
  foldlWithId' (\gs eid _ ->
                  case findId eid (gsEntities gs) of
                    Just e ->  foldl' (flip applyCommand) gs (eTick is gs eid e)
                    Nothing -> gs)
               startGs
               (gsEntities startGs)

collideEntities :: GameState -> GameState
collideEntities startGs =
  let collisions = [ (fst es, fst os)
                   | es <- foldlWithId' (\es i e -> (i,e):es) [] (gsEntities startGs)
                   , os <- foldlWithId' (\es i e -> (i,e):es) [] (gsEntities startGs)
                   , any (`elem` (eCollisionGroups $ snd os)) (eCollidesWith $ snd es)
                   , boxOverlaps (eBox $ snd es) (eBox $ snd os) ]
  in foldl' (\gs (eid, oid) ->
              case (findId eid (gsEntities gs), findId oid (gsEntities gs)) of
                (Just e, Just o) -> foldl' (flip applyCommand) gs (eCollide gs oid o eid e)
                _ -> gs)
           startGs
           collisions
