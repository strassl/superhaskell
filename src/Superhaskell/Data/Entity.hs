{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Superhaskell.Data.Entity (
    Style(..), Behavior(..), Entity(..)
) where

import           Control.DeepSeq
import           Data.Text
import           GHC.Generics
import           Superhaskell.Math

data Style = BoxStyle Text | NilStyle deriving (Show, Generic, NFData)
data Behavior = NoopBehavior deriving (Show, Generic, NFData)
data Entity = Entity { box :: Box, style :: Style, behavior :: Behavior }
            deriving (Show, Generic, NFData)
