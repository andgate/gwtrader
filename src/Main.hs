module Main where

import Item
import Listing
import Recipe
import ApiDbService

main :: IO ()
main = do
  updateServiceDb ItemService
  updateServiceDb CommerceService
  updateServiceDb RecipeService
