module Superhaskell.Processing (tickGameState, gravity, tps, viewPort) where

import qualified Data.Map.Strict              as Map
import           Control.Lens
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
tickEntities is gs = foldrWithId (tickEntity is) gs (gsEntities gs)

tickEntity :: InputState -> Id -> Entity -> GameState -> GameState
tickEntity is eid e gs = eTick is gs eid e

collideEntities :: GameState -> GameState
collideEntities gs =
  let entities = foldrWithId f Map.empty (gsEntities gs)
                   where f eid e m = foldr (g eid e) m [minBound..]
                         g eid e cg m =
                           if collidesWith cg (eCollisionGroup e)
                             then Map.insertWith (\[n] o -> n:o) cg [(eid, e)] m
                             else m 
  in foldrWithId (\sid s gs ->
                    let (gs', s') = applyCollisions gs sid s (collideEntity entities s)
                    in gs'{gsEntities=replaceId sid s' (gsEntities gs')})
                 gs
                 (gsEntities gs)

-- | Finds collitions with an entity.
collideEntity :: Map.Map CollisionGroup [(Id, Entity)] -> Entity -> [(Id, Entity)]
collideEntity others e = filter (boxOverlaps (eBox e) . eBox . snd)
                                (Map.findWithDefault [] (eCollisionGroup e) others)

-- | Applys all collitions to a given subject with all given objects.
applyCollisions :: GameState -> Id -> Entity -> [(Id, Entity)] -> (GameState, Entity)
applyCollisions gs eid e = foldr (applyCollision eid) (gs, e)

-- | Applys the collision and is supposed to return an updated version of the
-- subject entity. The second entity is the object entity.
applyCollision :: Id -> (Id, Entity) -> (GameState, Entity) -> (GameState, Entity)
applyCollision eid (oid, other) (gs, e) = eCollide oid other gs eid e
