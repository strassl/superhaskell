{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Superhaskell.Data.Entities (
    EntitiesC
  , makeEntities
  , esPlayer
  , filterOthers
  , appendOthers
) where

import           Control.DeepSeq
import           GHC.Generics
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

data EntitiesC a = EntitiesC Int a (HashMap Int a)
                 deriving (Show, Generic, NFData)

instance Foldable EntitiesC where
  foldr f zero (EntitiesC _ elem0 m) = f elem0 (foldr f zero m)

instance Functor EntitiesC where
  fmap f (EntitiesC nextId elem0 m) = EntitiesC nextId (f elem0) (fmap f m)

instance Traversable EntitiesC where
  traverse f (EntitiesC nextId elem0 m) = EntitiesC nextId <$> f elem0 <*> traverse f m

filterOthers :: (a -> Bool) -> EntitiesC a -> EntitiesC a
filterOthers p (EntitiesC nextId elem0 m) = EntitiesC nextId elem0 (M.filter p m)

insertOther :: a -> EntitiesC a -> EntitiesC a
insertOther e (EntitiesC nextId elem0 m) = EntitiesC (nextId + 1) elem0 (M.insert nextId e m)

appendOthers :: Foldable t => t a -> EntitiesC a -> EntitiesC a
appendOthers ts es = foldr insertOther es ts

makeEntities :: a -> EntitiesC a
makeEntities player = EntitiesC 1 player M.empty

esPlayer :: EntitiesC a -> a
esPlayer (EntitiesC _ elem0 _) = elem0
