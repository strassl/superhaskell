module Superhaskell.Processing (tickGameState, gravity) where

import           Control.Lens
import qualified Data.Map.Strict              as Map
import           Linear
import           Superhaskell.Data.Entities
import           Superhaskell.Data.GameState
import           Superhaskell.Data.InputState
import           Superhaskell.Math

-- Gravity in units/tick².
gravity :: Float
gravity = 0.5 / 60

-- Advances the game state by one tick.
tickGameState :: InputState -> GameState -> GameState
tickGameState is gs =
  if isWantQuit is
    then gs{ gsRunning = False }
    else (moveViewPort . collideEntities . tickEntities is) gs

moveViewPort :: GameState -> GameState
moveViewPort gs@GameState{gsViewPort = (Box _ wh)} = gs{gsViewPort = Box nlt wh}
  where player = esPlayer $ gsEntities gs
        nlt = over _xy (\v -> v-(wh / 2)) (boxAnchor $ eBox player)

tickEntities :: InputState -> GameState -> GameState
tickEntities is gs = foldrWithId (tickEntity is) gs (gsEntities gs)

tickEntity :: InputState -> Id -> Entity -> GameState -> GameState
tickEntity is eid e gs =
  let (gs', e') = eTick is gs eid e
  in gs'{gsEntities=replaceId eid e' (gsEntities gs')}

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
