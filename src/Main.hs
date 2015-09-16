module Main where

import ClassyPrelude

import Market

main :: IO ()
main = do
  args <- getArgs
  parseArgs args

parseArgs :: [Text] -> IO ()
parseArgs ("sync":args) =
  syncMarket

parseArgs ("rebuild":args) =
  buildMarket

parseArgs (cmd:args) =
  putStrLn $ "Unknown command '" ++ cmd ++ "' given."

parseArgs [] =
  putStrLn "No command given."
