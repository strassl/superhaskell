module Superhaskell.Game (run) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.Loops
import           Superhaskell.Data
import           Superhaskell.SDL.Rendering (SDLState, executeRenderList,
                                             initRendering)
import           Superhaskell.SDL.Input (getInputState)
import           Superhaskell.Generation
import           Superhaskell.Processing

run :: IO ()
run = do
  putStrLn "SUPERHASKELL"
  putStrLn "============"
  -- Init shared data boxes
  inputState <- getInputState
  let gameState = getInitialGameState
  inputStateBox <- atomically $ newTVar inputState;
  gameStateBox <- atomically $ newTVar gameState;
  -- Init SDL
  -- loadAssets
  sdlState <- initRendering
  -- Spawn game thread
  forkIO $ runGameLoop gameStateBox inputStateBox
  runRenderLoop gameStateBox inputStateBox sdlState
  putStrLn "Bye!"

runRenderLoop :: TVar GameState -> TVar InputState -> SDLState -> IO ()
runRenderLoop gameStateBox inputStateBox sdlState = do
  inputState <- getInputState
  atomicWrite inputStateBox inputState
  gameState <- atomicRead gameStateBox
  -- Generate renderlist here somehow
  executeRenderList sdlState []
  unless (not $ running gameState) $ runRenderLoop gameStateBox inputStateBox sdlState

runGameLoop :: TVar GameState -> TVar InputState -> IO ()
runGameLoop gameStateBox inputStateBox = do
  init <- atomicRead gameStateBox
  iterateUntilM_ (not . running) (stepGame gameStateBox inputStateBox) init

stepGame :: TVar GameState -> TVar InputState -> GameState -> IO GameState
stepGame gameStateBox inputStateBox gameState = do
    input <- atomicRead inputStateBox
    let gameState' = updateWorld gameState
    let gameState'' = tickGameState input gameState'
    -- Eww ugly, just a workaround for now
    let gameState''' = gameState'' { running = (running gameState'') && (not $ wantQuit input) }
    atomicWrite gameStateBox gameState''' -- seq!
    return gameState'''

getInitialGameState = GameState { running = True }

atomicWrite :: TVar a -> a -> IO ()
atomicWrite box val = atomically (writeTVar box val)

atomicRead :: TVar a -> IO a
atomicRead = atomically . readTVar

iterateUntilM_ :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
iterateUntilM_ p f i = iterateUntilM p f i >> return ()

{-
loadAssets = notImpl

runGameLoop :: IO ()
runGameLoop = loop stepGame getInitialGameState


render :: IO ()
render = do
    inputState <- getSDLState
    storeInputState inputState
    gameState <- getGameState
    R.render gameState

getSDLState = notImpl

getGameState = notImpl
storeGameState = notImpl

-- Repeated bind
loop :: Monad m => (a -> m a) -> a -> m a
loop step init = (loop step) =<< (step init)
-}
