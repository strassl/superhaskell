{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Game (run) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Linear                       (V2 (..), V3 (..))
import           Superhaskell.Data.Entity
import           Superhaskell.Data.GameState
import           Superhaskell.Data.InputState
import           Superhaskell.Generation
import           Superhaskell.Processing
import           Superhaskell.RenderList
import           Superhaskell.SDL.Input       (getInputState)
import           Superhaskell.SDL.Rendering   (SDLState, executeRenderList,
                                               initRendering)
import qualified System.Clock                 as C
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
  _ <- forkIO $ runGameLoop $ GameLoopState gameStateBox initialGameState inputStateBox 0 startTime
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
toRenderList gs = map toRenderCommand (entities gs) ++
                  [ RenderSprite "sun1" (Box (V3 4 4 0) (V2 1 1))
                  , RenderSprite "powerup_bubble" (Box (V3 2 1 0) (V2 1 1))]
  where toRenderCommand e = RenderSprite "pink" (box e)


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
