module Rendering
    ( initRendering
    , render
    ) where

import qualified World as W
import Util

initRendering :: IO ()
initRendering = notImpl

render :: W.GameState -> IO ()
render state = do
    let renderList = generateRenderList state -- extrapolate missed time
    executeRenderList renderList
    swapBuffers

executeRenderList = notImpl
generateRenderList = notImpl
swapBuffers = notImpl
