module Superhaskell.SDL.Input (
  SDLInputState, initInput, getInputState
) where

import           Control.Monad
import           Data.Int
import qualified Data.Vector                  as V
import           Linear
import           SDL
import           Superhaskell.Data.InputState
import           Superhaskell.Math
import           Superhaskell.SDL.Rendering

data SDLInputState =
  SDLInputState { sdlsJoystick       :: Maybe Joystick
                , sdlsRenderingState :: SDLRenderingState
                }

initInput :: SDLRenderingState -> IO SDLInputState
initInput rs = do
  joysticks <- availableJoysticks
  joystick <- if V.null joysticks
    then do
      print "No joystick :("
      return Nothing
    else do
      print (V.head joysticks)
      Just <$> openJoystick (V.head joysticks)

  return $ SDLInputState joystick rs

-- | Updates the current input state.
getInputState :: SDLInputState -> InputState -> IO InputState
getInputState state is = do
  events <- pollEvents
  wantQuit <- foldM (handleEvent state) False events

  keyboardState <- getKeyboardState
  joystickState <- getJoystickButtonsState (sdlsJoystick state)
  let kDir = keyboardDirection keyboardState
  jDir <- getJoystickDirection (sdlsJoystick state)
  return is{ isWantQuit = isWantQuit is || wantQuit
           , isDirection = if kDir == V2 0 0 then jDir else kDir
           , isJump = keyboardState ScancodeSpace || joystickState 0
           , isDrop = keyboardState ScancodeS || joystickState 2
           , isBoost = keyboardState ScancodeLShift || joystickState 1
           , isRestart = keyboardState ScancodeR || joystickState 0
           }

handleEvent :: SDLInputState -> Bool -> Event -> IO Bool
handleEvent _ _ (Event _ (WindowClosedEvent _)) =
  return True
handleEvent is wantQuit (Event _ (WindowResizedEvent _)) =
  onWindowResize (sdlsRenderingState is) >> return wantQuit
handleEvent _ wantQuit _ =
  return wantQuit

getJoystickButtonsState :: Maybe Joystick -> IO (Int -> Bool)
getJoystickButtonsState Nothing = return $ const False
getJoystickButtonsState (Just j) = do
  buttons <- fromIntegral <$> numButtons j
  states <- V.generateM buttons (buttonPressed j . fromIntegral)
  return (\i -> (i < V.length states) && (states V.! i))

getJoystickDirection :: Maybe Joystick -> IO (V2 Float)
getJoystickDirection Nothing = return $ V2 0 0
getJoystickDirection (Just j) = do
  axes <- numAxes j
  if axes < 2
    then return $ V2 0 0
    else do
      x <- axisPositionToFloat <$> axisPosition j 0
      y <- axisPositionToFloat <$> axisPosition j 1
      let v = V2 x y
      let v' = if norm v > 1 then normalize v else v
      return v'

axisPositionToFloat :: Int16 -> Float
axisPositionToFloat p =
  let p' = fromIntegral p / 32767
  in if abs p' < eps then 0 else p'

keyboardDirection :: (Scancode -> Bool) -> V2 Float
keyboardDirection keyboard =
  let left = keyboard ScancodeLeft || keyboard ScancodeA
      right = keyboard ScancodeRight || keyboard ScancodeD
      up = keyboard ScancodeUp || keyboard ScancodeW
      down = keyboard ScancodeDown || keyboard ScancodeS
      v = V2 ((if left then -1 else 0) + (if right then 1 else 0))
             ((if up then 1 else 0) + (if down then -1 else 0))
  in normalize v
