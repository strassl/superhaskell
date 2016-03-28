{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Superhaskell.Data.Entity (
    Style(..), Behavior(..), CollisionGroup(..), Entity(..), collidesWith
) where

import           Control.DeepSeq
import           Data.Text
import           GHC.Generics
import           Superhaskell.Math

data Style = BoxStyle Text
           | NilStyle
           deriving (Show, Generic, NFData)

data Behavior = NoopBehavior
              | PlayerBehavior
              deriving (Show, Generic, NFData)

data CollisionGroup = PlayerCGroup
                    | SceneryCGroup
                    | NilCGroup
                    deriving (Show, Generic, NFData)

data Entity = Entity { eBox            :: Box
                     , eStyle          :: Style
                     , eBehavior       :: Behavior
                     , eCollisionGroup :: CollisionGroup }
            deriving (Show, Generic, NFData)

collidesWith :: CollisionGroup -> CollisionGroup -> Bool
collidesWith PlayerCGroup SceneryCGroup = True
collidesWith _ _ = False
