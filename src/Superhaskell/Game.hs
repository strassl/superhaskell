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

data RenderLoopState = RenderLoopState { rlsGameStateBox  :: TVar GameState
                                       , rlsInputStateBox :: TVar InputState
                                       , rlsInputState    :: InputState
                                       , rlsSdlState      :: SDLState
                                       , rlsCount         :: Int
                                       , rlsStartTime     :: Double }

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
  forkIO $ runGameLoop $ GameLoopState gameStateBox initialGameState inputStateBox 0 startTime
  runRenderLoop $ RenderLoopState gameStateBox inputStateBox defaultInputState sdlState 0 startTime
  putStrLn "Bye!"

runRenderLoop :: RenderLoopState -> IO ()
runRenderLoop rls = do
  (rls', keepGoing) <- renderStep rls
  when keepGoing (runRenderLoop rls')

renderStep :: RenderLoopState -> IO (RenderLoopState, Bool)
renderStep rls = do
  now <- getTimeSeconds
  let timeDelta = now - rlsStartTime rls
  rls <- if timeDelta > 5 then do
    printFPS rls timeDelta
    return rls{rlsCount = 0, rlsStartTime = now}
  else
    return rls

  inputState <- getInputState (rlsInputState rls)
  atomicWrite (rlsInputStateBox rls) inputState  -- TODO seq?
  gameState <- atomicRead (rlsGameStateBox rls)
  executeRenderList (rlsSdlState rls) (toRenderList gameState)

  return (rls{ rlsCount = rlsCount rls + 1
            , rlsInputState = inputState }, running gameState)

toRenderList :: GameState -> RenderList
toRenderList _ = [ RenderSprite "sun1" Nothing (V3 400 400 0)
                 , RenderSprite "powerup_bubble" Nothing (V3 200 100 0)]

printFPS :: RenderLoopState -> Double -> IO ()
printFPS rls timeDelta = do
    let frames = fromIntegral (rlsCount rls)
    let fps = frames / timeDelta :: Double
    let mspf = timeDelta / frames * 1000 :: Double
    printf "Frame time: %.0f ms (%.1f FPS)\n" mspf fps
  

-- TODO output theoretical TPS and sleep time to measure performance
runGameLoop :: GameLoopState -> IO ()
runGameLoop gls = do
  now <- getTimeSeconds
  inputState <- atomicRead (glsInputStateBox gls)

  let todoTime = glsTimeLeft gls + (now - glsPrevTime gls)
  let todoIterations = floor $ todoTime / tickTime :: Int
  let gameState = foldr (const $ tickGame inputState)
                        (glsGameState gls)
                        [1..todoIterations]
  atomicWrite (glsGameStateBox gls) gameState  -- TODO seq!

  let todoTimeLeft = todoTime - fromIntegral todoIterations * tickTime
  let sleepUs = round $ (tickTime - todoTimeLeft) * 1000000
  when (sleepUs > 1000) $
    threadDelay sleepUs

  when (running gameState) $
    runGameLoop gls{ glsGameState = gameState
                   , glsPrevTime = now
                   , glsTimeLeft = todoTimeLeft }

tickTime :: Floating f => f
tickTime = 1 / 60

tickGame :: InputState -> GameState -> GameState
tickGame is = checkWantQuit is . tickGameState is . updateWorld

checkWantQuit :: InputState -> GameState -> GameState
checkWantQuit is gs = gs { running = running gs && not (wantQuit is) }

atomicWrite :: TVar a -> a -> IO ()
atomicWrite box val = atomically (writeTVar box val)

atomicRead :: TVar a -> IO a
atomicRead = atomically . readTVar

getTimeSeconds :: Floating f => IO f
getTimeSeconds = do
  now <- C.getTime C.Monotonic
  return $ fromIntegral (C.sec now) + fromIntegral (C.nsec now) / 1000000000
