module Superhaskell.Data.InputState (
    InputState(..), defaultInputState
) where


data InputState = InputState { wantQuit :: Bool} deriving Show

defaultInputState :: InputState
defaultInputState = InputState False
