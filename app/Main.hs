module Main (main) where

import           Superhaskell.Game
import           System.Console.GetOpt
import           System.Environment

data Flag = Debug | Bench deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  let (opts, nonopts, unknown, errors) = getOpt' Permute options args
  if any (not . null) [nonopts, unknown, errors]
    then do
      mapM_ putStrLn errors
      putStrLn $ usageInfo "superhaskell-exe" options
    else
      run (Debug `elem` opts) (Bench `elem` opts)

options :: [OptDescr Flag]
options =
  [ Option ['d'] ["debug"] (NoArg Debug) "enable debug features (e.g. OpenGL debug context)"
  , Option ['b'] ["bench"] (NoArg Bench) "disable VSync"
  ]
