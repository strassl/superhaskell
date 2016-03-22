module Superhaskell.Data.Entity (
    Style(..), Behavior(..), Entity(..)
) where

import           Superhaskell.Math

data Style = BoxStyle deriving Show
data Behavior = NoopBehavior deriving Show
data Entity = Entity { box :: Box, style :: Style, behavior :: Behavior } deriving Show
