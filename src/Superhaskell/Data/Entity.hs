module Superhaskell.Data.Entity (
    Style(..), Behavior(..), Entity(..)
) where

import Data.Text
import Superhaskell.Math

data Style = BoxStyle Text | NilStyle deriving Show
data Behavior = NoopBehavior deriving Show
data Entity = Entity { box :: Box, style :: Style, behavior :: Behavior } deriving Show
