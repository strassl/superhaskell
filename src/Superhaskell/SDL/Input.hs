{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.SDL.Input (
  getInputState
) where

import           Linear
import           SDL
import           Superhaskell.Data.InputState

-- | Updates the current input state.
getInputState :: InputState -> IO InputState
getInputState is = do
  events <- pollEvents
  keyboardState <- getKeyboardState
  return is{ isWantQuit = isWantQuit is || any isQuit events
           , isDirection = keyboardDirection keyboardState }

isQuit :: Event -> Bool
isQuit (Event _ (WindowClosedEvent _)) = True
isQuit _ = False

keyboardDirection :: (Scancode -> Bool) -> V2 Float
keyboardDirection keyboard =
  let left = keyboard ScancodeLeft || keyboard ScancodeA
      right = keyboard ScancodeRight || keyboard ScancodeD
      up = keyboard ScancodeUp || keyboard ScancodeW
      down = keyboard ScancodeDown || keyboard ScancodeS
      v = V2 ((if left then -1 else 0) + (if right then 1 else 0))
             ((if up then 1 else 0) + (if down then -1 else 0))
  in normalize v
