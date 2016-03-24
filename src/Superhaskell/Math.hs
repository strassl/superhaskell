{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Superhaskell.Math (Box(..)) where

import           Control.DeepSeq
import           GHC.Generics
import           Linear

data Box = Box { boxAnchor :: V3 Float
               , boxSize   :: V2 Float }
         deriving (Show, Generic, NFData)
