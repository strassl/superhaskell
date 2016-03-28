{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Game (run) where

import           Control.Monad.Random
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable
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
                                   , glsRandomGen     :: StdGen
                                   }

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
  let randGen = mkStdGen 43
  _ <- forkIO $ runGameLoop startTime 0 startTime 0 0 (GameLoopState gameStateBox
                                                                     initialGameState
                                                                     inputStateBox
                                                                     randGen)
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
  return (rls{rlsInputState = inputState}, gsRunning gameState)

printFPS :: Int -> Double -> IO ()
printFPS frames timeDelta = do
  let fps = fromIntegral frames / timeDelta
  let mspf = timeDelta / fromIntegral frames * 1000
  printf "Frame time: %.0f ms    %.1f FPS\n" mspf fps

runGameLoop :: Double -> Double -> Double -> Int -> Int -> GameLoopState -> IO ()
runGameLoop prevTime timeLeft measureStartTime totalSleepUs ticks gls = do
  now <- getTimeSeconds

  let todoTime = timeLeft + (now - prevTime)
  let todoIterations = floor $ todoTime / tickTime
  gls' <- gameStep gls todoIterations

  let todoTimeLeft = todoTime - fromIntegral todoIterations * tickTime
  let sleepUs = round $ (tickTime - todoTimeLeft) * 1000000
  (measureStartTime', totalSleepUs', ticks') <- do
    let delta = now - measureStartTime
    if delta > 5
      then
        if sleepUs > 1000
          then do
            printTPS delta totalSleepUs ticks
            threadDelay sleepUs
            return (now, sleepUs, todoIterations)
          else do
            printTPS delta totalSleepUs ticks
            return (now, 0, todoIterations)
      else
        if sleepUs > 1000
          then do
            threadDelay sleepUs
            return (measureStartTime, totalSleepUs + sleepUs, ticks + todoIterations)
          else
            return (measureStartTime, totalSleepUs, ticks + todoIterations)

  when (gsRunning $ glsGameState gls') $ runGameLoop now
                                                     todoTimeLeft
                                                     measureStartTime'
                                                     totalSleepUs'
                                                     ticks'
                                                     gls'

printTPS :: Double -> Int -> Int -> IO ()
printTPS deltaTime totalSleepUs ticks =
  let sleepTime = fromIntegral totalSleepUs / 1000000
      tickTime = (deltaTime - sleepTime) / fromIntegral ticks
      ttps = deltaTime / tickTime
  in printf "Sleep time: %.0f ms    Tick time: %.0f ms    %.1f theoretical TPS\n"
            (sleepTime * 1000)
            tickTime
            ttps

gameStep :: GameLoopState -> Int -> IO GameLoopState
gameStep gls iterations = do
  inputState <- atomicRead (glsInputStateBox gls)
  let (gameState, g) = runRand (iterateTimesM iterations (tickGame inputState) (glsGameState gls)) (glsRandomGen gls)
  gameState `deepseq` atomicWrite (glsGameStateBox gls) gameState
  return gls{ glsGameState = gameState
            , glsRandomGen = g}

iterateTimesM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateTimesM iterations f init = foldrM (const f) init [1..iterations]

tickTime :: Floating f => f
tickTime = 1 / 60

tickGame :: RandomGen g => InputState -> GameState -> Rand g GameState
tickGame is gs = fmap (tickGameState is) (updateWorld gs)

atomicWrite :: TVar a -> a -> IO ()
atomicWrite box val = atomically (writeTVar box val)

atomicRead :: TVar a -> IO a
atomicRead = atomically . readTVar

getTimeSeconds :: Floating f => IO f
getTimeSeconds = do
  now <- C.getTime C.Monotonic
  return $ fromIntegral (C.sec now) + fromIntegral (C.nsec now) / 1000000000
