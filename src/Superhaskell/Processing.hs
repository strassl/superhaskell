module Superhaskell.Processing (tickGameState, gravity) where

import           Control.Applicative
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

-- TODO !!!! allow GameState updates in eTick
tickEntities :: InputState -> GameState -> GameState
tickEntities is gs = gs{gsEntities=fmap (snd . eTick is gs) (gsEntities gs)}

-- TODO !!!! allow GameState updates in eCollide
collideEntities :: GameState -> GameState
collideEntities gs =
  let entities = foldr f Map.empty (gsEntities gs)
                   where f e m = foldr (g e) m [minBound..]
                         g e cg m =
                           if collidesWith cg (eCollisionGroup e)
                             then Map.insertWith (\[n] o -> n:o) cg [e] m
                             else m
      entities' = fmap (\s -> applyCollisions s (collideEntity entities s)) (gsEntities gs)
  in gs{gsEntities=entities'}

collideEntity :: Map.Map CollisionGroup [Entity] -> Entity -> [Entity]
collideEntity others e = filter (boxOverlaps (eBox e) . eBox)
                                (Map.findWithDefault [] (eCollisionGroup e) others)

applyCollisions :: Entity -> [Entity] -> Entity
applyCollisions = foldr applyCollision

-- | Applys the collision and is supposed to return an updated version of the
-- *second* entity (the subject). The second entity is the object.
applyCollision :: Entity -> Entity -> Entity
applyCollision o e = snd $ eCollide o undefined e
