{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Drawing (toRenderList) where

import Superhaskell.Data.Entity
import Superhaskell.Data.GameState
import Superhaskell.RenderList

toRenderList :: GameState -> RenderList
toRenderList gs = map toRenderCommand (entities gs)
  where toRenderCommand e = RenderSprite "pink" (box e)
