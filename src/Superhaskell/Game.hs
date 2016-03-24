{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Game (run) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Monad
import           Linear
import           Superhaskell.Data.GameState
import           Superhaskell.Data.InputState
import           Superhaskell.Drawing
import           Superhaskell.Generation
import           Superhaskell.Processing
import           Superhaskell.SDL.Input       (getInputState)
import           Superhaskell.SDL.Rendering   (SDLState, executeRenderList,
                                               initRendering)
import qualified System.Clock                 as C
import           Text.Printf

data RenderLoopState = RenderLoopState { rlsGameStateBox  :: TVar GameState
                                       , rlsInputStateBox :: TVar InputState
                                       , rlsInputState    :: InputState
                                       , rlsSdlState      :: SDLState }

data GameLoopState = GameLoopState { glsGameStateBox  :: TVar GameState
                                   , glsGameState     :: GameState
                                   , glsInputStateBox :: TVar InputState
                                   , glsTimeLeft      :: Double
                                   , glsPrevTime      :: Double }

run :: IO ()
run = do
  putStrLn "SUPERHASKELL"
  putStrLn "============"
  -- Init SDL
  sdlState <- initRendering
  -- Init shared data boxes
  inputStateBox <- atomically $ newTVar defaultInputState
  gameStateBox <- atomically $ newTVar initialGameState
  -- Spawn game thread
  startTime <- getTimeSeconds
  _ <- forkIO $ runGameLoop $ GameLoopState gameStateBox initialGameState inputStateBox 0 startTime
  runRenderLoop startTime 0 (RenderLoopState gameStateBox
                                             inputStateBox
                                             defaultInputState
                                             sdlState)
  putStrLn "Bye!"

runRenderLoop :: Double -> Int -> RenderLoopState -> IO ()
runRenderLoop fpsStartTime fpsCount rls = do
  now <- getTimeSeconds
  let timeDelta = now - fpsStartTime
  (fpsStartTime', fpsCount') <- if timeDelta > 5 then do
    printFPS fpsCount timeDelta
    return (now, 0)
  else
    return (fpsStartTime, fpsCount + 1)

  (rls', keepGoing) <- renderStep rls
  when keepGoing (runRenderLoop fpsStartTime' fpsCount' rls')

renderStep :: RenderLoopState -> IO (RenderLoopState, Bool)
renderStep rls = do
  inputState <- getInputState (rlsInputState rls)
  inputState `deepseq` atomicWrite (rlsInputStateBox rls) inputState
  gameState <- atomicRead (rlsGameStateBox rls)
  executeRenderList (rlsSdlState rls) (V2 16 9) (toRenderList gameState)
  return (rls{rlsInputState = inputState}, running gameState)

printFPS :: Int -> Double -> IO ()
printFPS frames timeDelta = do
  let fps = fromIntegral frames / timeDelta
  let mspf = timeDelta / fromIntegral frames * 1000
  printf "Frame time: %.0f ms (%.1f FPS)\n" mspf fps

-- TODO output theoretical TPS and sleep time to measure performance
runGameLoop :: GameLoopState -> IO ()
runGameLoop gls = do
  now <- getTimeSeconds

  let todoTime = glsTimeLeft gls + (now - glsPrevTime gls)
  let todoIterations = floor $ todoTime / tickTime :: Int
  gls' <- gameStep gls todoIterations

  let todoTimeLeft = todoTime - fromIntegral todoIterations * tickTime
  let sleepUs = round $ (tickTime - todoTimeLeft) * 1000000
  when (sleepUs > 1000) $
    threadDelay sleepUs

  when (running $ glsGameState gls') $
    runGameLoop gls'{ glsPrevTime = now
                    , glsTimeLeft = todoTimeLeft }

gameStep :: GameLoopState -> Int -> IO GameLoopState
gameStep gls iterations = do
  inputState <- atomicRead (glsInputStateBox gls)
  let gameState = iterateTimes iterations (tickGame inputState) (glsGameState gls)
  atomicWrite (glsGameStateBox gls) gameState  -- TODO seq!

  return gls{ glsGameState = gameState }

iterateTimes :: Int -> (a -> a) -> a -> a
iterateTimes iterations f init = foldr (const f) init [1..iterations]

tickTime :: Floating f => f
tickTime = 1 / 60

tickGame :: InputState -> GameState -> GameState
tickGame is = tickGameState is . updateWorld

atomicWrite :: TVar a -> a -> IO ()
atomicWrite box val = atomically (writeTVar box val)

atomicRead :: TVar a -> IO a
atomicRead = atomically . readTVar

getTimeSeconds :: Floating f => IO f
getTimeSeconds = do
  now <- C.getTime C.Monotonic
  return $ fromIntegral (C.sec now) + fromIntegral (C.nsec now) / 1000000000
