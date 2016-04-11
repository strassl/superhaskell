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
    KeyFrames
  , KeyFrame(..)
  , RenderCommand(..)
  , RenderList
) where

import           Data.Text         (Text)
import           Superhaskell.Math

type KeyFrames = [KeyFrame]

data KeyFrame = KeyFrame { kfRenderList :: RenderList
                         , kfDuration   :: Float -- in s
                         }

data RenderCommand = RenderSprite Text      -- ^ Name of the texture
                                  Box       -- ^ Position
                                  Float     -- ^ Z
                   deriving (Show)

type RenderList = [RenderCommand]
