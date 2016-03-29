{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Superhaskell.Data.GameState (
    GameState(..),
    GenState(..),
    gsEntityList, stopGame,
    GameParams(..),
    initialGameState,
    entitiesAt, entitiesAtInGroup,

    Game, liftGame, runGame,
    askParams, asksParams,
    askInput, asksInput,
    logMsg,
    askGameState, asksGameState, modifyGameState, modifyGameStateM
) where

import           Data.Foldable
import           Control.DeepSeq
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Text                (Text)
import qualified Data.Text                as T
import           GHC.Generics
import           Linear.V2                (V2 (..))
import           Linear.V3                (V3 (..))
import           Superhaskell.Data.Entity
import           Superhaskell.Data.Entities
import           Superhaskell.Math
import Superhaskell.Data.InputState

data GameParams = GameParams ()

data GameState = GameState { gsEntities :: Entities
                           , gsRunning  :: Bool
                           , gsGenState :: GenState
                           , gsViewPort :: Box
                           }
               deriving (Show, Generic, NFData)

data GenState = GenState { genBound :: Float
                         }
               deriving (Show, Generic, NFData)

initialGameState :: GameState
initialGameState = GameState { gsRunning = True
                             , gsEntities = makeEntities player
                             , gsGenState = initialGenState
                             , gsViewPort = Box (V3 0 0 0) (V2 16 9)
                             }

gsEntityList :: GameState -> [Entity]
gsEntityList = toList . gsEntities

-- Stores information that the generation component needs across iterations
-- Such as up to where it already generated the world
initialGenState :: GenState
initialGenState = GenState { genBound = 0.0}

entitiesAt :: V2 Float -> GameState -> [Entity]
entitiesAt p gs = filter (boxContains p . eBox) (gsEntityList gs)

entitiesAtInGroup :: V2 Float -> CollisionGroup -> GameState -> [Entity]
entitiesAtInGroup p g gs = filter ((== g) . eCollisionGroup) (entitiesAt p gs)

stopGame :: GameState -> GameState
stopGame gs = gs{gsRunning=False}

player :: Entity
player = Entity { eBox = Box (V3 4 2 0) (V2 0.5970149253731343 1)
                , eStyle = BoxStyle "bunny1_stand"  -- TODO PlayerStyle
                , eBehavior = PlayerBehavior { bvFalling = Just 0 }
                , eCollisionGroup = PlayerCGroup }

newtype Game a = Game { unGame :: StateT GameState
                                ( WriterT Text
                                ( ReaderT InputState
                                ( ReaderT GameParams
                                  Identity))) a }

-- Equivalent to MonadIO for our monad.
class Monad m => MonadGame m where
  liftGame :: MonadGame m => Game a -> m a

instance Functor Game where
  fmap fn = Game . fmap fn . unGame

instance Applicative Game where
  pure = Game . pure
  a <*> b = Game $ unGame a <*> unGame b

instance Monad Game where
  return = pure
  a >>= b = Game $ unGame a >>= (unGame . b)

instance MonadGame Game where
  liftGame = id

runGame :: Game a -> GameState -> InputState -> GameParams -> (a, GameState, Text)
runGame m gameState inputState params =
  let ((a, state'), log) = runIdentity (runReaderT (runReaderT (runWriterT (runStateT (unGame m) gameState)) inputState) params)
  in (a, state', log)

askParams :: MonadGame m => m GameParams
askParams = liftGame $ Game $ (lift . lift . lift) ask

asksParams :: MonadGame m => (GameParams -> a) -> m a
asksParams fn = liftGame $ Game $ (lift . lift . lift) (asks fn)

askInput :: MonadGame m => m InputState
askInput = liftGame $ Game $ (lift . lift) ask

asksInput :: MonadGame m => (InputState -> a) -> m a
asksInput fn = liftGame $ Game $ (lift . lift) (asks fn)

logMsg :: MonadGame m => Text -> m ()
logMsg msg = liftGame $ Game $ tell msg

askGameState :: MonadGame m => m GameState
askGameState = liftGame $ Game get

asksGameState :: MonadGame m => (GameState -> a) -> m a
asksGameState fn = liftGame $ Game $ gets fn

modifyGameState :: MonadGame m => (GameState -> GameState) -> m ()
modifyGameState fn = liftGame $ Game $ modify fn

modifyGameStateM :: MonadGame m => (GameState -> m GameState) -> m ()
modifyGameStateM fn = do
  gs <- liftGame $ Game get
  gs' <- fn gs
  liftGame $ Game $ put gs'
