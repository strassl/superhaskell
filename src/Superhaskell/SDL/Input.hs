{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.SDL.Input (
  InputState(..), getInputState
) where

import SDL

data InputState = InputState { wantQuit :: Bool} deriving Show

getInputState :: IO InputState
getInputState = do
  events <- pollEvents
  return $ InputState { wantQuit = any isQuit events }

isQuit :: Event -> Bool
isQuit (Event _ (WindowClosedEvent _)) = True
isQuit _ = False
