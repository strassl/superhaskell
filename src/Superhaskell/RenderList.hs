module Superhaskell.RenderList (
  RenderCommand(..), RenderList
) where

import           Data.Text (Text)
import           Linear    (V2, V3)

data RenderCommand = RenderSprite !Text !(Maybe (V2 Float)) !(V3 Float)
                   deriving (Show)

type RenderList = [RenderCommand]

