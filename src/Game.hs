module Game
    ( 
     run
    ) where

import Control.Concurrent

import qualified Generation as G
import qualified Processing as P
import qualified Rendering as R
import qualified World as W
import Util

run :: IO ()
run = do
    R.initRendering
    loadAssets
    forkIO $ runGameLoop
    runRenderLoop

loadAssets = notImpl

runGameLoop :: IO ()
runGameLoop = loop stepGame getInitialGameState

stepGame :: a -> IO ()
stepGame gameState = do
    input <- getInputState
    let gameState' = G.updateWorld gameState
    let gameState'' = P.tickGameState input gameState'
    storeGameState gameState'' -- seq!)

runRenderLoop :: IO ()
runRenderLoop = loop (\_ -> render) ()

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
