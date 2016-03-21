{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.SDL.Input (
  getInputState
) where

import SDL
import Superhaskell.Data (InputState(..))

getInputState :: InputState -> IO InputState
getInputState is = do
  events <- pollEvents
  return is{wantQuit = wantQuit is || any isQuit events}

isQuit :: Event -> Bool
isQuit (Event _ (WindowClosedEvent _)) = True
isQuit _ = False
