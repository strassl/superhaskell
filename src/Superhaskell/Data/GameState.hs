{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs          #-}
module Superhaskell.Data.GameState (
    GameState(..), entitiesAt, entitiesAtInGroup, gsEntityList, toRenderList
  , GenState(..), initialGenState -- TODO remove initialGenState
  , CollisionGroup(..), collidesWith
  , IsEntity(..), Entity, Entities
) where

import           Control.DeepSeq
import           Data.Foldable
import           GHC.Generics
import           Linear.V2                    (V2 (..))
import           Linear.V3                    (V3 (..))
import           Superhaskell.Data.Entities
import           Superhaskell.Data.InputState
import           Superhaskell.Data.RenderList
import           Superhaskell.Math

class (Show e, NFData e) => IsEntity e where
  eTick :: InputState -> GameState -> e -> (GameState, e)
  eRender :: GameState -> e -> RenderList
  eCollide :: IsEntity o => o -> GameState -> e -> (GameState, e)
  eCollisionGroup :: e -> CollisionGroup
  eBox :: e -> Box
  eWrap :: e -> Entity

  eTick _ = (,)
  eRender _ _ = []
  eCollide _ = (,)
  eCollisionGroup _ = NilCGroup
  eBox _ = Box (V3 0 0 0) (V2 0 0)
  eWrap e = Entity e

data Entity where
  Entity :: IsEntity e => e -> Entity

instance Show Entity where
  show (Entity e) = "(Entity $ " ++ show e ++ ")"

instance NFData Entity where
  rnf (Entity e) = rnf e

instance IsEntity Entity where
  eTick is gs (Entity e) = let (gs', e') = eTick is gs e in (gs', Entity e')
  eRender gs (Entity e) = eRender gs e
  eCollide other gs (Entity e) = let (gs', e') = eCollide other gs e in (gs', Entity e')
  eCollisionGroup (Entity e) = eCollisionGroup e
  eBox (Entity e) = eBox e
  eWrap = id

type Entities = EntitiesC Entity

data CollisionGroup = PlayerCGroup
                    | SceneryCGroup
                    | NilCGroup
                    deriving (Show, Generic, NFData, Eq, Enum, Bounded, Ord)

collidesWith :: CollisionGroup -> CollisionGroup -> Bool
collidesWith PlayerCGroup SceneryCGroup = True
collidesWith _ _ = False

data GameState = GameState { gsEntities :: Entities
                           , gsRunning  :: Bool
                           , gsGenState :: GenState
                           , gsViewPort :: Box
                           }
               deriving (Show, Generic, NFData)

data GenState = GenState { genBound :: Float
                         }
               deriving (Show, Generic, NFData)

gsEntityList :: GameState -> [Entity]
gsEntityList = toList . gsEntities

-- Stores information that the generation component needs across iterations
-- Such as up to where it already generated the world
initialGenState :: GenState
initialGenState = GenState { genBound = 0.0}

entitiesAt :: V2 Float -> GameState -> [Entity]
entitiesAt p gs = filter (boxContains p . eBox) (gsEntityList gs)

entitiesAtInGroup :: V2 Float -> CollisionGroup -> GameState -> [Entity]
entitiesAtInGroup p g gs = filter ((== g) . eCollisionGroup) (entitiesAt p gs)

toRenderList :: GameState -> RenderList
toRenderList gs = concatMap (eRender gs) (gsEntityList gs)
