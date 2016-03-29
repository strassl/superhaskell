module Superhaskell.Processing (tickGameState) where

import           Control.Applicative
import           Control.Lens
import qualified Data.Map.Strict              as Map
import           Linear
import           Superhaskell.Data.Entity
import           Superhaskell.Data.Entities
import           Superhaskell.Data.GameState
import           Superhaskell.Data.InputState
import           Superhaskell.Math
import Data.Traversable

-- Base speed in units/tick.
playerBaseSpeed :: Float
playerBaseSpeed = 3 / 60

-- Gravity in units/tick².
gravity :: Float
gravity = 0.5 / 60

-- A small number.
eps :: Float
eps = 1 / 1024

tickGameState :: Game ()
tickGameState = do
  wantQuit <- asksInput isWantQuit
  if wantQuit
    then modifyGameState stopGame
    else tickEntities >> collideEntities >> modifyGameState moveViewPort

moveViewPort :: GameState -> GameState
moveViewPort gs@GameState{gsViewPort = (Box lt wh)} = gs{gsViewPort = Box nlt wh}
  where player = esPlayer $ gsEntities gs
        nlt = over _xy (\v -> v-(wh / 2)) (boxAnchor $ eBox player)

tickEntities :: Game ()
tickEntities = modifyGameStateM $ \gs -> do
  entities' <- mapM tickEntity (gsEntities gs)
  return gs{gsEntities=entities'}

tickEntity :: Entity -> Game Entity
tickEntity e@Entity{eBox=box, eBehavior=behavior} = do
  (box', behavior') <- applyBehavior box behavior
  return e{eBox=box', eBehavior=behavior'}

applyBehavior :: Box -> Behavior -> Game (Box, Behavior)
applyBehavior box bv@PlayerBehavior{bvFalling=falling} = do
  (V2 inputX _) <- asksInput isDirection
  gs <- askGameState
  let (falling', gravityDeltaPos) = applyGravity falling
      moveDeltaPos = V2 (inputX * playerBaseSpeed) 0
      box' = moveBox (gravityDeltaPos ^+^ moveDeltaPos) box
      offEdge =  null (entitiesAtInGroup (leftBottom box + V2 0 eps) SceneryCGroup gs)
              && null (entitiesAtInGroup (rightBottom box + V2 0 eps) SceneryCGroup gs)
  return (box', bv{bvFalling=falling' <|> if offEdge then Just 1 else Nothing})
applyBehavior box b =
  return (box, b)

applyGravity :: Maybe Float -> (Maybe Float, V2 Float)
applyGravity (Just time) = (Just (time + 1), V2 0 (gravity * time))
applyGravity Nothing = (Nothing, V2 0 0)

collideEntities :: Game ()
collideEntities =
  modifyGameStateM $ \gs -> do
    let entities = foldr f Map.empty (gsEntities gs)
                     where f e m = foldr (g e) m [minBound..]
                           g e cg m =
                             if collidesWith cg (eCollisionGroup e)
                               then Map.insertWith (\[n] o -> n:o) cg [e] m
                               else m
    entities' <- mapM (\s -> applyCollisions s (collideEntity entities s)) (gsEntities gs)
    return gs{gsEntities=entities'}

collideEntity :: Map.Map CollisionGroup [Entity] -> Entity -> [Entity]
collideEntity others e = filter (boxOverlaps (eBox e) . eBox)
                                (Map.findWithDefault [] (eCollisionGroup e) others)

applyCollisions :: Entity -> [Entity] -> Game Entity
applyCollisions e os =
  let (box', behavior') =
        foldr (\Entity{eBox=oBox, eBehavior=oBv} (box, bv) -> applyCollision oBox oBv box bv)
              (eBox e, eBehavior e)
              os
  in e{eBox=box', eBehavior=behavior'}

-- | Applys the collision and is supposed to return an updated version of the
-- *second* entity (the subject). The second entity is the object.
applyCollision :: Box -> Behavior -> Box -> Behavior -> Game (Box, Behavior)
applyCollision obox _ box bv@PlayerBehavior{} =
  return (pushOut obox box, bv{bvFalling=Nothing})
applyCollision _ _ box e =
  return (box, e)
