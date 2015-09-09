module Main where

import Market
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  readArgs args

readArgs :: [String] -> IO ()
readArgs ("sync":args) =
  syncMarket

readArgs (cmd:args) =
  putStrLn $ "Unknown command '" ++ cmd ++ "' given."

readArgs [] =
  putStrLn "No command given."
