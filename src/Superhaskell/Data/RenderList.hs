-- Coordinate System:
--          -y
--          ^
--          |
--          |
-- -x <-----+-----> +x
--          |
--          |
--          v
--          +y
-- The background is -z, the foreground is +z.
--
-- 0/0 is the topleft corner of the viewing area.
--
-- 1 ph is one player height.
module Superhaskell.Data.RenderList (
  RenderCommand(..), RenderList
) where

import           Data.Text         (Text)
import           Superhaskell.Math

data RenderCommand = RenderSprite Text -- ^ Name of the texture
                                  Box  -- ^ Position
                   deriving (Show)

type RenderList = [RenderCommand]
