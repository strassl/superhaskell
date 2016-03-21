{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Game (run) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Loops
import           Linear                     (V3 (..))
import           Superhaskell.Data
import           Superhaskell.Generation
import           Superhaskell.Processing
import           Superhaskell.RenderList
import           Superhaskell.SDL.Input     (getInputState)
import           Superhaskell.SDL.Rendering (SDLState, executeRenderList,
                                             initRendering)
import qualified System.Clock               as C
import           Text.Printf

data RenderLoopState = RenderLoopState { rlsGameState  :: TVar GameState
                                       , rlsInputState :: TVar InputState
                                       , rlsSdlState   :: SDLState
                                       , rlsCount      :: Int
                                       , rlsStartTime  :: Double }

run :: IO ()
run = do
  putStrLn "SUPERHASKELL"
  putStrLn "============"
  -- Init SDL
  sdlState <- initRendering
  -- Init shared data boxes
  inputState <- getInputState
  inputStateBox <- atomically $ newTVar inputState
  gameStateBox <- atomically $ newTVar initialGameState
  -- Spawn game thread
  startTime <- getTimeSeconds
  forkIO $ runGameLoop gameStateBox inputStateBox
  runRenderLoop $ RenderLoopState gameStateBox inputStateBox sdlState 0 startTime
  putStrLn "Bye!"

runRenderLoop :: RenderLoopState -> IO ()
runRenderLoop rls = do
  now <- getTimeSeconds
  let timeDelta = now - rlsStartTime rls
  rls <- if timeDelta > 5 then do
    let frames = fromIntegral (rlsCount rls)
    let fps = frames / timeDelta :: Double
    let mspf = timeDelta / frames * 1000 :: Double
    printf "Frame time: %.0f ms (%.1f FPS)\n" mspf fps
    return rls{rlsCount = 0, rlsStartTime = now}
  else
    return rls

  inputState <- getInputState
  atomicWrite (rlsInputState rls) inputState  -- TODO seq?
  gameState <- atomicRead (rlsGameState rls)
  executeRenderList (rlsSdlState rls)
                    [ RenderSprite "sun1" Nothing (V3 400 400 0)
                    , RenderSprite "powerup_bubble" Nothing (V3 200 100 0)]

  when (running gameState) $
    runRenderLoop rls{rlsCount = rlsCount rls + 1}

runGameLoop :: TVar GameState -> TVar InputState -> IO ()
runGameLoop gameStateBox inputStateBox = do
  init <- atomicRead gameStateBox
  iterateUntilM_ (not . running) (stepGame gameStateBox inputStateBox) init

stepGame :: TVar GameState -> TVar InputState -> GameState -> IO GameState
stepGame gameStateBox inputStateBox gameState = do
  input <- atomicRead inputStateBox
  let gameState' = updateWorld gameState
  let gameState'' = tickGameState input gameState'
  -- TODO Eww ugly, just a workaround for now
  let gameState''' = gameState'' { running = running gameState'' && not (wantQuit input) }
  atomicWrite gameStateBox gameState''' -- TODO seq!
  return gameState'''

initialGameState :: GameState
initialGameState = GameState { running = True }

atomicWrite :: TVar a -> a -> IO ()
atomicWrite box val = atomically (writeTVar box val)

atomicRead :: TVar a -> IO a
atomicRead = atomically . readTVar

iterateUntilM_ :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
iterateUntilM_ p f i = void (iterateUntilM p f i)

getTimeSeconds :: Floating f => IO f
getTimeSeconds = do
  now <- C.getTime C.Monotonic
  return $ fromIntegral (C.sec now) + fromIntegral (C.nsec now) / 1000000000
