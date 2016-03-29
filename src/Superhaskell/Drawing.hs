{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Drawing (toRenderList) where

import Superhaskell.Data.Entity
import Superhaskell.Data.GameState
import Superhaskell.RenderList
import Data.Text (Text)

toRenderList :: GameState -> RenderList
toRenderList gs = map toRenderCommand (gsEntityList gs)

toRenderCommand :: Entity -> RenderCommand
toRenderCommand e = RenderSprite (getTexture (eStyle e)) (eBox e)

getTexture :: Style -> Text
getTexture (BoxStyle t) = t
getTexture _ = defaultTexture

defaultTexture :: Text
defaultTexture = "pink"
