{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.SDL.Input (
  getInputState
) where

import SDL
import Superhaskell.Data (InputState(..))

getInputState :: IO InputState
getInputState = do
  events <- pollEvents
  return $ InputState (any isQuit events)

isQuit :: Event -> Bool
isQuit (Event _ (WindowClosedEvent _)) = True
isQuit _ = False
