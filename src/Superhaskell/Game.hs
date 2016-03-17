module Superhaskell.Game (run) where

import           Control.Concurrent
import           Control.Monad
import           Superhaskell.SDL.Rendering (SDLState, executeRenderList,
                                             initRendering)

run :: IO ()
run = do
  putStrLn "SUPERHASKELL"
  putStrLn "============"
  sdlState <- initRendering
  --loadAssets
  -- forkIO $ runGameLoop
  runRenderLoop sdlState
  putStrLn "Bye!"

runRenderLoop :: SDLState -> IO ()
runRenderLoop sdlState = do
  wantQuit <- executeRenderList sdlState []
  unless wantQuit $ runRenderLoop sdlState

{-
loadAssets = notImpl

runGameLoop :: IO ()
runGameLoop = loop stepGame getInitialGameState

stepGame :: a -> IO ()
stepGame gameState = do
    input <- getInputState
    let gameState' = G.updateWorld gameState
    let gameState'' = P.tickGameState input gameState'
    storeGameState gameState'' -- seq!)


render :: IO ()
render = do
    inputState <- getSDLState
    storeInputState inputState
    gameState <- getGameState
    R.render gameState

getSDLState = notImpl

getInputState = notImpl
storeInputState = notImpl

getInitialGameState = notImpl
getGameState = notImpl
storeGameState = notImpl

-- Repeated bind
loop :: Monad m => (a -> m a) -> a -> m a
loop step init = (loop step) =<< (step init)
-}
