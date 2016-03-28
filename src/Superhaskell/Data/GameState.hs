{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
module Superhaskell.Data.GameState (
    GameState(..), initialGameState
) where

import           Control.DeepSeq
import           GHC.Generics
import           Superhaskell.Data.Entity

data GameState = GameState { entities :: [Entity]
                           , running  :: Bool }
               deriving (Show, Generic, NFData)

initialGameState :: GameState
initialGameState = GameState { running = True
                             , entities = [] }

