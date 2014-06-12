module Main where

import Picologic.Repl
import Control.Monad
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> void $ runRepl repl
    ["-"]   -> stdin
    [fname] -> void $ runRepl (file fname >> repl)
    _ -> putStrLn "invalid arguments"
