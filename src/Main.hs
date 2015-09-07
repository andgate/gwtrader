module Main where

import Item
import Listing
import Recipe

main :: IO ()
main = do
  items <- buildTPItemList
  print items
