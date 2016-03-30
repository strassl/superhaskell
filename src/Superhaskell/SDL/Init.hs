module Superhaskell.SDL.Init (initSDL) where

import qualified SDL
import           Superhaskell.SDL.Input
import           Superhaskell.SDL.Rendering

initSDL :: IO (SDLRenderingState, SDLInputState)
initSDL = do
  SDL.initializeAll
  rs <- initRendering
  is <- initInput
  return (rs, is)
