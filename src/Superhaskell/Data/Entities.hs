{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Superhaskell.Data.Entities (
    Entities()
  , makeEntities
  , esPlayer
  , filterOthers
  , appendOthers
) where

import           Control.DeepSeq
import           GHC.Generics
import           Superhaskell.Data.Entity

type Entities = EntitiesC Entity
data EntitiesC a = EntitiesC { esPlayer :: a
                             , esOthers :: [a]
                             }
            deriving (Show, Generic, NFData)

instance Foldable EntitiesC where
  foldr f z _es@EntitiesC{ esPlayer = player, esOthers = os} = foldr f z (player:os)

instance Functor EntitiesC where
  fmap f es@EntitiesC{ esPlayer = player, esOthers = os} = es { esPlayer = f player, esOthers = fmap f os}

instance Traversable EntitiesC where
  traverse f _es@EntitiesC{ esPlayer = player, esOthers = os} = EntitiesC <$> f player <*> traverse f os

filterOthers :: (a -> Bool) -> EntitiesC a -> EntitiesC a
filterOthers p es@EntitiesC{ esOthers = os} = es { esOthers = filter p os}

appendOthers :: Foldable t => EntitiesC a -> t a -> EntitiesC a
appendOthers es@EntitiesC{ esOthers = os} ts = es { esOthers = nOthers }
  where nOthers = foldr (:) os ts

makeEntities :: Entity -> Entities
makeEntities player = EntitiesC { esPlayer = player, esOthers = []}
