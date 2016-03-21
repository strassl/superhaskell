{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Game (run) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.Loops
import           Linear                      (V3 (..))
import           Superhaskell.Data
import           Superhaskell.Generation
import           Superhaskell.Processing
import           Superhaskell.RenderList
import           Superhaskell.SDL.Input      (getInputState)
import           Superhaskell.SDL.Rendering  (SDLState, executeRenderList,
                                              initRendering)

run :: IO ()
run = do
  putStrLn "SUPERHASKELL"
  putStrLn "============"
  -- Init SDL
  sdlState <- initRendering
  -- Init shared data boxes
  inputState <- getInputState
  let gameState = getInitialGameState
  inputStateBox <- atomically $ newTVar inputState;
  gameStateBox <- atomically $ newTVar gameState;
  -- Spawn game thread
  forkIO $ runGameLoop gameStateBox inputStateBox
  runRenderLoop gameStateBox inputStateBox sdlState
  putStrLn "Bye!"

runRenderLoop :: TVar GameState -> TVar InputState -> SDLState -> IO ()
runRenderLoop gameStateBox inputStateBox sdlState = do
  inputState <- getInputState
  atomicWrite inputStateBox inputState
  gameState <- atomicRead gameStateBox
  executeRenderList sdlState [ RenderSprite "sun1" Nothing (V3 400 400 0)
                             , RenderSprite "powerup_bubble" Nothing (V3 200 100 0)]
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
  let gameState''' = gameState'' { running = running gameState'' && not (wantQuit input) }
  atomicWrite gameStateBox gameState''' -- seq!
  return gameState'''

getInitialGameState = GameState { running = True }

atomicWrite :: TVar a -> a -> IO ()
atomicWrite box val = atomically (writeTVar box val)

atomicRead :: TVar a -> IO a
atomicRead = atomically . readTVar

iterateUntilM_ :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
iterateUntilM_ p f i = void (iterateUntilM p f i)
