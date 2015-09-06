module Main where

import Item

main :: IO ()
main = do
  items <- buildItemList
  print items
