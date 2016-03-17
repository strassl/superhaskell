{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.SDL.Rendering (
  SDLState, initRendering, executeRenderList
) where

import           Linear                  (V4 (..))
import           SDL
import           Superhaskell.RenderList

data SDLState = SDLState { sdlsWindow :: Window, sdlsRenderer :: Renderer }

initRendering :: IO SDLState
initRendering = do
  initializeAll
  window <- createWindow "Superhaskell" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  return $ SDLState window renderer

executeRenderList :: SDLState -> RenderList -> IO Bool
executeRenderList state rl =
  let r = sdlsRenderer state
  in do
    events <- pollEvents
    let wantQuit = any isQuit events
    rendererDrawColor r $= V4 0 0 0 255
    clear r
    present r
    return wantQuit

isQuit :: Event -> Bool
isQuit (Event _ (WindowClosedEvent _)) = True
isQuit _ = False
