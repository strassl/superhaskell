module Lib
    ( someFunc,
      double
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

double :: Int -> Int
double = (*) 2
