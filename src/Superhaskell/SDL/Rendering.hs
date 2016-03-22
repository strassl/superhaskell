{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.SDL.Rendering (
  SDLState, initRendering, executeRenderList
) where

import           Codec.Picture           (convertRGBA8, imageData, imageHeight,
                                          imageWidth, readImage)
import           Control.Arrow           ((&&&))
import           Control.Monad
import qualified Data.HashMap.Strict     as M
import           Data.List               (sortBy)
import           Data.Maybe              (fromJust)
import qualified Data.Text               as T
import qualified Data.Vector.Storable    as VS
import           Foreign.C.Types
import           Linear
import           Linear.Affine           (Point (..))
import           SDL
import           Superhaskell.Math
import           Superhaskell.RenderList
import           System.Directory        (getDirectoryContents)

type Textures = M.HashMap T.Text Texture

data SDLState = SDLState { sdlsRenderer :: Renderer
                         , sdlsTextures :: Textures }

initRendering :: IO SDLState
initRendering = do
  initializeAll
  window <- createWindow "Superhaskell" defaultWindow{windowInitialSize=V2 1280 720}
  renderer <- createRenderer window (-1) defaultRenderer
  textures <- loadTextures renderer
  return $ SDLState renderer textures

loadTextures :: Renderer -> IO Textures
loadTextures renderer = do
  files <-     map (("assets/textures/" ++) &&& takeWhile (/= '.'))
             . filter ((/= '.') . head)
           <$> getDirectoryContents "assets/textures/"
  foldM (loadTexture renderer) M.empty files

loadTexture :: Renderer -> Textures -> (String, String) -> IO Textures
loadTexture renderer textures (path, name) = do
  image <- readImage path
  case image of
    Right image -> do
      putStrLn $ "Loading texture " ++ path
      let rgbaImage = convertRGBA8 image
      let size = V2 (fromIntegral $ imageWidth rgbaImage) (fromIntegral $ imageHeight rgbaImage)
      pixelData <- VS.unsafeThaw (imageData rgbaImage)
      surface <- createRGBSurfaceFrom pixelData
                                      size
                                      32
                                      (fromIntegral $ imageWidth rgbaImage * 4)
                                      (V4 0x000000ff 0x0000ff00 0x00ff0000 0xff000000)
      texture <- createTextureFromSurface renderer surface
      freeSurface surface
      return $ M.insert (T.pack name) texture textures
    Left error -> do
      putStrLn $ "Could not load texture " ++ path ++ ": " ++ error
      return textures

executeRenderList :: SDLState -> RenderList -> IO ()
executeRenderList state renderList = do
  let r = sdlsRenderer state
  rendererDrawColor r $= V4 0 0 0 255
  clear r
  forM_ (sortBy renderCommandOrd renderList) (executeRenderCommand state)
  present r

executeRenderCommand :: SDLState -> RenderCommand -> IO ()
executeRenderCommand state (RenderSprite tex box) =
  let size = boxSize box
      (V3 x y _) = boxAnchor box
  in
    copy (sdlsRenderer state)
         (fromJust $ M.lookup tex (sdlsTextures state))
         Nothing
         (Just $ Rectangle (P (V2 (phToPx x) (phToPx y))) (fmap phToPx size))

phToPx :: Float -> CInt
phToPx = round . (* 60)

renderCommandOrd :: RenderCommand -> RenderCommand -> Ordering
renderCommandOrd (RenderSprite _ Box{boxAnchor=(V3 _ _ a)})
                 (RenderSprite _ Box{boxAnchor=(V3 _ _ b)}) = compare a b
