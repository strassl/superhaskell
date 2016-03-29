{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Data.GameState (
    GameState(..),
    GenState(..),
    gsEntityList,
    initialGameState,
    entitiesAt,
    entitiesAtInGroup
) where

import           Data.Foldable
import           Control.DeepSeq
import           GHC.Generics
import           Superhaskell.Data.Entity
import           Superhaskell.Data.Entities
import           Superhaskell.Math
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))

data GameState = GameState { gsEntities :: Entities
                           , gsRunning  :: Bool
                           , gsGenState :: GenState
                           , gsViewPort :: Box
                           }
               deriving (Show, Generic, NFData)

data GenState = GenState { genBound :: Float
                         }
               deriving (Show, Generic, NFData)

initialGameState :: GameState
initialGameState = GameState { gsRunning = True
                             , gsEntities = makeEntities player
                             , gsGenState = initialGenState
                             , gsViewPort = Box (V3 0 0 0) (V2 16 9)
                             }

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

player :: Entity
player = Entity { eBox = Box (V3 4 2 0) (V2 0.5970149253731343 1)
                , eStyle = BoxStyle "bunny1_stand"  -- TODO PlayerStyle
                , eBehavior = PlayerBehavior { bvFalling = Just 0 }
                , eCollisionGroup = PlayerCGroup }
