{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Game (run) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Random
import           Data.Foldable
import           Linear
import           Superhaskell.Data.Entities
import           Superhaskell.Data.GameState
import           Superhaskell.Data.InputState
import           Superhaskell.Entities.Platform
import           Superhaskell.Entities.Player
import           Superhaskell.Entities.ScoreCounter
import           Superhaskell.Generation
import           Superhaskell.Math
import           Superhaskell.Processing
import           Superhaskell.SDL.Init          (initSDL)
import           Superhaskell.SDL.Input         (SDLInputState, getInputState)
import           Superhaskell.SDL.Rendering     (SDLRenderingState,
                                                 executeRenderList)
import qualified System.Clock                   as C
import           Text.Printf

data RenderLoopState = RenderLoopState { rlsGameStateBox  :: TVar GameState
                                       , rlsInputStateBox :: TVar InputState
                                       , rlsInputState    :: InputState
                                       , rlsSdlRState     :: SDLRenderingState
                                       , rlsSdlIState     :: SDLInputState }

data GameLoopState = GameLoopState { glsGameStateBox  :: TVar GameState
                                   , glsGameState     :: GameState
                                   , glsInputStateBox :: TVar InputState
                                   , glsRandomGen     :: StdGen
                                   }

initialGameState :: GameState
initialGameState = GameState { gsRunning = True
                             , gsEntities = ents
                             , gsGenState = initialGenState
                             , gsViewPort = Box (V2 0 0) viewPort
                             }
  where p = eWrap player
        initPlatform = eWrap $ platform (boxAnchor (eBox p) + V2 0 4) 6
        counter = eWrap $ scoreCounter (V2 0 0)
        ents = appendOthers [counter, initPlatform] $ makeEntities p

run :: Bool -> Bool -> IO ()
run debug bench = do
  putStrLn "SUPERHASKELL"
  putStrLn "============"
  -- Init SDL
  (sdlRState, sdlIState) <- initSDL debug bench
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
                                             sdlRState
                                             sdlIState)
  putStrLn "Bye!"

runRenderLoop :: Float -> Int -> RenderLoopState -> IO ()
runRenderLoop fpsStartTime fpsCount rls = do
  now <- getTimeSeconds
  let timeDelta = now - fpsStartTime
  (fpsStartTime', fpsCount') <- if timeDelta > 5 then do
    printFPS fpsCount timeDelta
    return (now, 0)
  else
    return (fpsStartTime, fpsCount + 1)

  (rls', keepGoing) <- renderStep rls now
  when keepGoing (runRenderLoop fpsStartTime' fpsCount' rls')

renderStep :: RenderLoopState -> Float -> IO (RenderLoopState, Bool)
renderStep rls time = do
  inputState <- getInputState (rlsSdlIState rls) (rlsInputState rls)
  inputState `deepseq` atomicWrite (rlsInputStateBox rls) inputState
  gameState <- atomicRead (rlsGameStateBox rls)
  executeRenderList (rlsSdlRState rls) (gsViewPort gameState) (toRenderList time gameState)
  return (rls{rlsInputState = inputState}, gsRunning gameState)

printFPS :: Int -> Float -> IO ()
printFPS frames timeDelta = do
  let fps = fromIntegral frames / timeDelta
  let mspf = timeDelta / fromIntegral frames * 1000
  printf "Frame time: %.0f ms    %.1f FPS\n" mspf fps

runGameLoop :: Float -> Float -> Float -> Int -> Int -> GameLoopState -> IO ()
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

printTPS :: Float -> Int -> Int -> IO ()
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

tickTime :: Float
tickTime = 1 / tps

iterateTimesM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateTimesM iterations f init = foldrM (const f) init [1..iterations]

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
