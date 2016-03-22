module Superhaskell.Processing (tickGameState) where

import Superhaskell.Data.GameState
import Superhaskell.Data.InputState

tickGameState :: InputState -> GameState -> GameState
tickGameState is gs = checkWantQuit is gs

checkWantQuit :: InputState -> GameState -> GameState
checkWantQuit is gs = gs { running = running gs && not (wantQuit is) }
