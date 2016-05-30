{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Superhaskell.Data.Entities (
    EntitiesC
  , Id
  , makeEntities
  , esPlayer
  , filterOthers
  , appendOthers
  , foldrWithId
  , mapWithId
  , replaceId
  , insertOther
  , removeId
  , findId
) where

import           Control.DeepSeq
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           GHC.Generics

newtype Id = Id Int deriving (Show, Eq)

data EntitiesC a = EntitiesC Int a (HashMap Int a)
                 deriving (Show, Generic, NFData)

instance Foldable EntitiesC where
  foldr f zero (EntitiesC _ elem0 m) = f elem0 (foldr f zero m)

instance Functor EntitiesC where
  fmap f (EntitiesC nextId elem0 m) = EntitiesC nextId (f elem0) (fmap f m)

instance Traversable EntitiesC where
  traverse f (EntitiesC nextId elem0 m) = EntitiesC nextId <$> f elem0 <*> traverse f m

foldrWithId :: (Id -> a -> b -> b) -> b -> EntitiesC a -> b
foldrWithId f zero (EntitiesC _ elem0 m) =
  f (Id 0) elem0 (M.foldrWithKey (\k e acc -> f (Id k) e acc) zero m)

mapWithId :: (Id -> a -> b) -> EntitiesC a -> EntitiesC b
mapWithId f (EntitiesC nextId elem0 m) =
  EntitiesC nextId (f (Id 0) elem0) (M.mapWithKey (\k e -> f (Id k) e) m)

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

replaceId :: Id -> a -> EntitiesC a -> EntitiesC a
replaceId (Id 0) e (EntitiesC nextId _ m) = EntitiesC nextId e m
replaceId (Id i) e (EntitiesC nextId elem0 m) =
  if i `M.member` m
    then EntitiesC nextId elem0 (M.insert i e m)
    else EntitiesC nextId elem0 m

removeId :: Id -> EntitiesC a -> EntitiesC a
removeId (Id 0) es = es
removeId (Id i) (EntitiesC nextId elem0 m) = EntitiesC nextId elem0 (M.delete i m)

findId :: Id -> EntitiesC a -> Maybe a
findId (Id 0) (EntitiesC _ elem0 _) = Just elem0
findId (Id i) (EntitiesC _ _ m) = i `M.lookup` m


