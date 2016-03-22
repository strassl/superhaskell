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
-- 1 is one player height
-- -> that are 120 texture pixels
-- -> that are 30 screen pixels in the default resolution
module Superhaskell.RenderList (
  RenderCommand(..), RenderList
) where

import           Data.Text (Text)
import           Superhaskell.Math

data RenderCommand = RenderSprite Text -- Name of the texture
                                  Box  -- Position
                   deriving (Show)

type RenderList = [RenderCommand]

