module Superhaskell.Processing (tickGameState) where

import           Control.Applicative
import qualified Data.Map.Strict              as Map
import           Linear
import           Superhaskell.Data.Entity
import           Superhaskell.Data.GameState
import           Superhaskell.Data.InputState
import           Superhaskell.Math

-- Base speed in units/tick.
playerBaseSpeed :: Float
playerBaseSpeed = 3 / 60

-- Gravity in units/tick².
gravity :: Float
gravity = 0.5 / 60

-- A small number.
eps :: Float
eps = 1 / 1024

-- Advances the game state by one tick.
tickGameState :: InputState -> GameState -> GameState
tickGameState is gs =
  if isWantQuit is
    then gs{ gsRunning = False }
    else (collideEntities . tickEntities is) gs

tickEntities :: InputState -> GameState -> GameState
tickEntities is gs = gs{ gsEntities = fmap (tickEntity is gs) (gsEntities gs) }

tickEntity :: InputState -> GameState -> Entity -> Entity
tickEntity is gs e@Entity{eBox=box, eBehavior=behavior} =
  let (box', behavior') = applyBehavior is gs box behavior
  in e{eBox=box', eBehavior=behavior'}

applyBehavior :: InputState -> GameState -> Box -> Behavior -> (Box, Behavior)
applyBehavior is gs box bv@PlayerBehavior{bvFalling=falling} =
  let (V2 inputX _) = isDirection is
      (falling', gravityDeltaPos) = applyGravity falling
      moveDeltaPos = V2 (inputX * playerBaseSpeed) 0
      box' = moveBox (gravityDeltaPos ^+^ moveDeltaPos) box
      offEdge =  null (entitiesAtInGroup (leftBottom box + V2 0 eps) SceneryCGroup gs)
              && null (entitiesAtInGroup (rightBottom box + V2 0 eps) SceneryCGroup gs)
  in (box', bv{bvFalling=falling' <|> if offEdge then Just 1 else Nothing})
applyBehavior _ _ box b = (box, b)

applyGravity :: Maybe Float -> (Maybe Float, V2 Float)
applyGravity (Just time) = (Just (time + 1), V2 0 (gravity * time))
applyGravity Nothing = (Nothing, V2 0 0)

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
applyCollisions e os =
  let (box', behavior') =
        foldr (\Entity{eBox=oBox, eBehavior=oBv} (box, bv) -> applyCollision oBox oBv box bv)
              (eBox e, eBehavior e)
              os
  in e{eBox=box', eBehavior=behavior'}

-- | Applys the collision and is supposed to return an updated version of the
-- *second* entity (the subject). The second entity is the object.
applyCollision :: Box -> Behavior -> Box -> Behavior -> (Box, Behavior)
applyCollision obox _ box bv@PlayerBehavior{} =
  (pushOut obox box, bv{bvFalling=Nothing})
applyCollision _ _ box e =
  (box, e)
