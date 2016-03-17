module Superhaskell.RenderList (
  RenderCommand(..), RenderList
) where

data RenderCommand = Blaa | Blaaaaa -- todo
                   deriving (Show)

type RenderList = [RenderCommand]
