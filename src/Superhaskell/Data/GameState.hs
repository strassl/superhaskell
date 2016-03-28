{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Data.GameState (
    GameState(..), initialGameState
) where

import           Control.DeepSeq
import           GHC.Generics
import           Linear.V2                (V2 (..))
import           Linear.V3                (V3 (..))
import           Superhaskell.Data.Entity
import           Superhaskell.Math

data GameState = GameState { gsEntities :: [Entity]
                           , gsRunning  :: Bool }
               deriving (Show, Generic, NFData)

initialGameState :: GameState
initialGameState = GameState { gsRunning = True
                             , gsEntities = mockEntities }

mockEntities :: [Entity]
mockEntities = [player, platform{eBox=Box (V3 2 8 (-1)) (V2 5 1)}]

platform :: Entity
platform = Entity { eBox = Box (V3 0 0 (-1)) (V2 0 0)
                  , eStyle = BoxStyle "ground_stone"
                  , eBehavior = NoopBehavior
                  , eCollisionGroup = SceneryCGroup }

silverCoin :: Entity
silverCoin = Entity { eBox = Box (V3 0 0 1) (V2 0 0)
                    , eStyle = BoxStyle "silver_1"
                    , eBehavior = NoopBehavior
                    , eCollisionGroup = NilCGroup }

player :: Entity
player = Entity { eBox = Box (V3 5 5 0) (V2 0.5970149253731343 1)
                , eStyle = BoxStyle "bunny1_stand"  -- TODO PlayerStyle
                , eBehavior = PlayerBehavior { bvFalling = Just 0 }
                , eCollisionGroup = PlayerCGroup }
