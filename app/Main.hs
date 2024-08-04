module Main where

import Metrics (directorySummary)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> directorySummary dir
    _ -> putStrLn "Usage: app <directory>"
