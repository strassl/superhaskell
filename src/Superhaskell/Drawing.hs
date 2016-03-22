{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Drawing (toRenderList) where

import Superhaskell.Data.Entity
import Superhaskell.Data.GameState
import Superhaskell.RenderList
import Data.Text (Text)

toRenderList :: GameState -> RenderList
toRenderList gs = map toRenderCommand (entities gs)

toRenderCommand :: Entity -> RenderCommand
toRenderCommand e = RenderSprite (getTexture (style e)) (box e)

getTexture :: Style -> Text
getTexture (BoxStyle t) = t
getTexture _ = defaultTexture

defaultTexture :: Text
defaultTexture = "pink"
