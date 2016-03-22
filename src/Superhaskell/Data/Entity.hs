module Superhaskell.Data.Entity (
    Style(..), Behavior(..), Box(..), Entity(..)
) where

import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))

data Style = BoxStyle deriving Show
data Behavior = NoopBehavior deriving Show
data Box = Box { anchor :: V3 Float, size :: V2 Float } deriving Show
data Entity = Entity { box :: Box, style :: Style, behavior :: Behavior } deriving Show
