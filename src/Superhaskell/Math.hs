module Superhaskell.Math (Box(..)) where

import           Linear

data Box = Box { boxAnchor :: V3 Float
               , boxSize   :: V2 Float }
         deriving Show
