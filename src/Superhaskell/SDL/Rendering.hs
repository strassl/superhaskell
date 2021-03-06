{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
module Superhaskell.SDL.Rendering (
  SDLRenderingState, initRendering, executeRenderList, onWindowResize
) where

import           Codec.Picture                (convertRGBA8, imageData,
                                               imageHeight, imageWidth,
                                               readImage)
import           Control.Applicative
import           Control.Arrow                ((&&&))
import           Control.Monad
import           Data.ByteString              (ByteString)
import qualified Data.HashMap.Strict          as M
import           Data.List                    (sortBy)
import qualified Data.Text                    as T
import qualified Data.Vector.Storable         as VS
import           Foreign.Marshal              (with)
import           Foreign.Ptr                  (castPtr, nullPtr)
import           Foreign.Storable             (sizeOf)
import           Graphics.GL                  (glUniformMatrix3fv)
import           Graphics.GL.ARB.DebugOutput
import           Graphics.Rendering.OpenGL    hiding (imageHeight)
import           Linear                       (V2 (..), V3 (..))
import qualified SDL
import           Superhaskell.Data.RenderList
import           Superhaskell.Math
import           System.Directory             (getDirectoryContents)
import           Text.RawString.QQ

newtype M33 = M33 (V3 (V3 Float)) deriving (VS.Storable)

instance Uniform M33 where
  uniform (UniformLocation l) =
    makeStateVar (error "not implemented")
                 (\value -> with value (glUniformMatrix3fv l 1 1 . castPtr))
  uniformv = error "not implemented"

type Textures = M.HashMap T.Text TextureObject

data SDLRenderingState =
  SDLRenderingState { sdlsWindow                  :: SDL.Window
                    , _sdlsContext                :: SDL.GLContext
                    , sdlsTextures                :: Textures
                    , _sdlsSpriteProgram          :: Program
                    , sdlsSpriteProgramUTransform :: UniformLocation
                    , sdlsSpriteProgramUCamera    :: UniformLocation
                    , _sdlsSpriteProgramUTexture  :: UniformLocation
                    , _sdlsUnitSquareVao          :: VertexArrayObject
                    , _sdlsUnitSquareVbo          :: BufferObject }

initRendering :: Bool -> Bool -> IO SDLRenderingState
initRendering debug bench = do
  window <- SDL.createWindow "Superhaskell"
                             SDL.defaultWindow{ SDL.windowInitialSize = V2 1280 720
                                              , SDL.windowResizable = True
                                              , SDL.windowOpenGL = Just SDL.defaultOpenGL{
                                                  SDL.glProfile = SDL.Core (if debug then SDL.Debug else SDL.Normal) 3 3 }}
  context <- SDL.glCreateContext window

  SDL.swapInterval $= if bench then SDL.ImmediateUpdates else SDL.SynchronizedUpdates

  supportsDebugOutput <- glGetARBDebugOutput
  when supportsDebugOutput $ do
    debugOutput $= Enabled
    debugMessageCallback $= Just print

  blend $= Enabled
  blendEquation $= FuncAdd
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  spriteProgram <- setupShaders
  spriteProgramUTransform <- get (uniformLocation spriteProgram "uTransform")
  spriteProgramUCamera <- get (uniformLocation spriteProgram "uCamera")
  spriteProgramUTexture <- get (uniformLocation spriteProgram "uTexture")

  (unitSquareVao, unitSquareVbo) <- setupUnitSquare

  textures <- loadTextures

  clearColor $= Color4 (0xAD / 0xFF) (0xE6 / 0xFF) 1 1
  currentProgram $= Just spriteProgram
  uniform spriteProgramUTexture $= TextureUnit 0
  bindVertexArrayObject $= Just unitSquareVao

  return $ SDLRenderingState window
                             context
                             textures
                             spriteProgram
                             spriteProgramUTransform
                             spriteProgramUCamera
                             spriteProgramUTexture
                             unitSquareVao
                             unitSquareVbo

onWindowResize :: SDLRenderingState -> IO ()
onWindowResize state = do
  (V2 w h) <- get $ SDL.windowSize (sdlsWindow state)
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

setupShaders :: IO Program
setupShaders = do
  boxVertexShader <- myCompileShader "boxVertexShader" VertexShader boxVertexShaderSource
  textureFragmentShader <- myCompileShader "textureFragmentShader" FragmentShader textureFragmentShaderSource
  spriteProgram <- myLinkProgram "spriteProgram" [boxVertexShader, textureFragmentShader]
  releaseShaderCompiler
  return spriteProgram

myCompileShader :: String -> ShaderType -> ByteString -> IO Shader
myCompileShader name type_ source = do
  shader <- createShader type_
  shaderSourceBS shader $= source
  compileShader shader
  success <- get (compileStatus shader)
  unless success $ do
    putStrLn $ "Error while compiling " ++ name
    putStrLn =<< get (shaderInfoLog shader)
    fail ""
  return shader

myLinkProgram :: String -> [Shader] -> IO Program
myLinkProgram name shaders = do
  program <- createProgram
  forM_ shaders (attachShader program)
  linkProgram program
  success <- get (linkStatus program)
  unless success $ do
    putStrLn $ "Error while linking " ++ name
    putStrLn =<< get (programInfoLog program)
    fail ""
  return program

setupUnitSquare :: IO (VertexArrayObject, BufferObject)
setupUnitSquare = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  let floats = VS.fromList [V2 0 0,
                            V2 0 1,
                            V2 1 0,
                            V2 1 (1 :: Float)]
  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo
  VS.unsafeWith floats $ \ptr ->
    bufferData ArrayBuffer $= (vectorBytes floats, ptr, StaticDraw)
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)
  vertexAttribArray (AttribLocation 0) $= Enabled

  bindVertexArrayObject $= Nothing
  bindBuffer ArrayBuffer $= Nothing
  return (vao, vbo)

loadTextures :: IO Textures
loadTextures = do
  files <-     map (("assets/textures/" ++) &&& takeWhile (/= '.'))
             . filter ((/= '.') . head)
           <$> getDirectoryContents "assets/textures/"
  foldM loadTexture M.empty files

loadTexture :: Textures -> (String, String) -> IO Textures
loadTexture textures (path, name) = do
  image <- readImage path
  case image of
    Right image -> do
      putStrLn $ "Loading texture " ++ path
      let rgbaImage = convertRGBA8 image
      let size = TextureSize2D (fromIntegral $ imageWidth rgbaImage)
                               (fromIntegral $ imageHeight rgbaImage)
      texture <- genObjectName
      textureBinding Texture2D $= Just texture
      VS.unsafeWith (imageData rgbaImage) $ \ptr ->
        texImage2D Texture2D NoProxy 0 RGBA' size 0 (PixelData RGBA UnsignedByte ptr)
      textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
      textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
      textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
      generateMipmap' Texture2D

      textureBinding Texture2D $= Nothing
      return $ M.insert (T.pack name) texture textures
    Left err ->
      fail $ "Could not load texture " ++ path ++ ": " ++ err

executeRenderList :: SDLRenderingState -> Box -> RenderList -> IO ()
executeRenderList sdls viewport@Box{boxAnchor=V2 x y, boxSize=V2 w h} renderList = do
  (x, y, w, h) <- do
    (V2 winW winH) <- get (SDL.windowSize $ sdlsWindow sdls)
    let winAspect = fromIntegral winW / fromIntegral winH
    let boxAspect = w / h
    return $ if winAspect > boxAspect
      then let h' = h * boxAspect / winAspect
               y' = y - (h' - h) / 2
           in (x, y', w, h')
      else let w' = w * winAspect / boxAspect
               x' = x - (w' - w) / 2
           in (x', y, w', h)

  clear [ColorBuffer]
  -- http://tinyurl.com/zkbr3zs
  uniform (sdlsSpriteProgramUCamera sdls) $=
    M33 (V3 (V3 (2/w) 0      (-2*x/w-1))
            (V3 0     (-2/h) (2*y/h+1))
            (V3 0     0      1))
  forM_ (sortBy compareRenderCommand renderList)
        (executeRenderCommand sdls viewport)
  SDL.glSwapWindow (sdlsWindow sdls)

executeRenderCommand :: SDLRenderingState -> Box -> RenderCommand -> IO ()
executeRenderCommand sdls _ (RenderSprite tex Box{boxAnchor=V2 x y, boxSize=V2 w h} _) = do
  -- http://tinyurl.com/znrp8uq
  uniform (sdlsSpriteProgramUTransform sdls) $=
    M33 (V3 (V3 w 0 x)
            (V3 0 h y)
            (V3 0 0 1))
  textureBinding Texture2D $= lookupTexture tex (sdlsTextures sdls)
  drawArrays TriangleStrip 0 4

compareRenderCommand :: RenderCommand -> RenderCommand -> Ordering
compareRenderCommand (RenderSprite _ _ a) (RenderSprite _ _ b) = compare a b

vectorBytes :: (Integral i, VS.Storable a) => VS.Vector a -> i
vectorBytes v = fromIntegral $ VS.length v * sizeOf (VS.unsafeHead v)

lookupTexture :: T.Text -> Textures -> Maybe TextureObject
lookupTexture tex map = M.lookup tex map <|> M.lookup pinkTexture map

pinkTexture :: T.Text
pinkTexture = "pink"

boxVertexShaderSource :: ByteString
boxVertexShaderSource =
  [r|
    #version 330

    layout(location = 0) in vec2 vPos;

    uniform mat3 uTransform;
    uniform mat3 uCamera;

    out vec2 oTexPos;

    void main()
    {
      vec2 pos = vec2(uCamera * (uTransform * vec3(vPos, 1)));
      gl_Position = vec4(pos, 0, 1);
      oTexPos = vPos;
    }
  |]

textureFragmentShaderSource :: ByteString
textureFragmentShaderSource =
  [r|
    #version 330

    in vec2 oTexPos;

    uniform sampler2D uTexture;

    out vec4 color;

    void main()
    {
      color = texture(uTexture, oTexPos);
    }
  |]
