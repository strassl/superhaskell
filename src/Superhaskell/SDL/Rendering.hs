{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
module Superhaskell.SDL.Rendering (
  SDLState, initRendering, executeRenderList
) where

import           Codec.Picture             (convertRGBA8, imageData,
                                            imageHeight, imageWidth, readImage)
import           Control.Arrow             ((&&&))
import           Control.Monad
import           Data.ByteString           (ByteString)
import qualified Data.HashMap.Strict       as M
import           Data.List                 (sortBy)
import qualified Data.Text                 as T
import qualified Data.Vector.Storable      as VS
import           Foreign.Marshal           (with)
import           Foreign.Ptr               (castPtr, nullPtr)
import           Foreign.Storable          (sizeOf)
import           Graphics.GL               (glUniformMatrix3fv)
import           Graphics.Rendering.OpenGL hiding (imageHeight)
import           Linear                    (V2 (..), V3 (..))
import qualified SDL
import           Superhaskell.Math
import           Superhaskell.RenderList
import           System.Directory          (getDirectoryContents)
import           Text.RawString.QQ

newtype M33 = M33 (V3 (V3 Float)) deriving (VS.Storable)

instance Uniform M33 where
  uniform (UniformLocation l) =
    makeStateVar (error "not implemented")
                 (\value -> with value (glUniformMatrix3fv l 1 1 . castPtr))
  uniformv = error "not implemented"

type Textures = M.HashMap T.Text TextureObject

data SDLState = SDLState { sdlsWindow         :: SDL.Window
                         , _sdlsContext       :: SDL.GLContext
                         , sdlsTextures       :: Textures
                         , _sdlsSpriteProgram :: Program
                         , _sdlsUnitSquareVao :: VertexArrayObject
                         , _sdlsUnitSquareVbo :: BufferObject }

initRendering :: IO SDLState
initRendering = do
  SDL.initializeAll
  window <- SDL.createWindow "Superhaskell"
                             SDL.defaultWindow{ SDL.windowInitialSize = V2 1280 720
                                              , SDL.windowOpenGL = Just SDL.defaultOpenGL{
                                                  SDL.glProfile = SDL.Core SDL.Debug 3 3 }}
  context <- SDL.glCreateContext window
  debugOutput $= Enabled
  debugMessageCallback $= Just print
  spriteProgram <- setupShaders
  (unitSquareVao, unitSquareVbo) <- setupUnitSquare
  textures <- loadTextures
  clearColor $= Color4 0 0 0 1
  currentProgram $= Just spriteProgram
  uniform uTexture $= TextureUnit 0
  bindVertexArrayObject $= Just unitSquareVao
  return $ SDLState window
                    context
                    textures
                    spriteProgram
                    unitSquareVao
                    unitSquareVbo

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
      textureWrapMode Texture2D S $= (Mirrored, Clamp)
      textureWrapMode Texture2D T $= (Mirrored, Clamp)
      textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
      generateMipmap' Texture2D

      textureBinding Texture2D $= Nothing
      return $ M.insert (T.pack name) texture textures
    Left err ->
      fail $ "Could not load texture " ++ path ++ ": " ++ err

executeRenderList :: SDLState -> V2 Float -> RenderList -> IO ()
executeRenderList sdls viewport renderList = do
  clear [ColorBuffer]
  forM_ (sortBy compareRenderCommand renderList)
        (executeRenderCommand sdls viewport)
  SDL.glSwapWindow (sdlsWindow sdls)

executeRenderCommand :: SDLState -> V2 Float -> RenderCommand -> IO ()
executeRenderCommand sdls (V2 u v) (RenderSprite tex Box{boxAnchor=V3 x y _, boxSize=V2 w h}) = do
    -- http://tinyurl.com/guuob3r
    uniform uTransform $= M33 (V3 (V3 (2*w/u) 0        (2*x/u - 1))
                                  (V3 0       (-2*h/v) (1 - 2*y/v))
                                  (V3 0       0        1))
    textureBinding Texture2D $= M.lookup tex (sdlsTextures sdls)
    drawArrays TriangleStrip 0 4

compareRenderCommand :: RenderCommand -> RenderCommand -> Ordering
compareRenderCommand (RenderSprite _ Box{boxAnchor=V3 _ _ a})
                     (RenderSprite _ Box{boxAnchor=V3 _ _ b}) = compare a b

vectorBytes :: (Integral i, VS.Storable a) => VS.Vector a -> i
vectorBytes v = fromIntegral $ VS.length v * sizeOf (VS.unsafeHead v)

uTransform :: UniformLocation
uTransform = UniformLocation 0

uTexture :: UniformLocation
uTexture = UniformLocation 1

boxVertexShaderSource :: ByteString
boxVertexShaderSource =
  [r|
    #version 330
    #extension ARB_explicit_uniform_location: require

    layout(location = 0) in vec2 vPos;

    layout(location = 0) uniform mat3 uTransform;

    out vec2 oTexPos;

    void main()
    {
      vec2 pos = vec2(uTransform * vec3(vPos, 1));
      gl_Position = vec4(pos, 0, 1);
      oTexPos = vPos;
    }
  |]

textureFragmentShaderSource :: ByteString
textureFragmentShaderSource =
  [r|
    #version 330
    #extension ARB_explicit_uniform_location: require

    in vec2 oTexPos;

    layout(location = 1) uniform sampler2D uTexture;

    void main()
    {
      gl_FragColor = texture(uTexture, oTexPos);
    }
  |]
