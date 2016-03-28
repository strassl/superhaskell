{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
module Superhaskell.Data.GameState (
    GameState(..),
    GenState(..),
    initialGameState
) where

import           Control.DeepSeq
import           GHC.Generics
import           Superhaskell.Data.Entity
import           Linear.V2 (V2(..))

data GameState = GameState { entities :: [Entity]
                           , running  :: Bool
                           , gsGenState :: GenState
                           , gsViewPort :: V2 Float
                           }
               deriving (Show, Generic, NFData)

data GenState = GenState { genBound :: Float
                         }
               deriving (Show, Generic, NFData)


initialGameState :: GameState
initialGameState = GameState { running = True
                             , entities = []
                             , gsGenState = initialGenState
                             , gsViewPort = V2 16 9
                             }

-- Stores information that the generation component needs across iterations
-- Such as up to where it already generated the world
initialGenState :: GenState
initialGenState = GenState { genBound = 0.0}
