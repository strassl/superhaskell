module Superhaskell.SDL.Init (initSDL) where

import qualified SDL
import           Superhaskell.SDL.Input
import           Superhaskell.SDL.Rendering

initSDL :: Bool -> Bool -> IO (SDLRenderingState, SDLInputState)
initSDL debug bench = do
  SDL.initializeAll
  rs <- initRendering debug bench
  is <- initInput rs
  return (rs, is)
