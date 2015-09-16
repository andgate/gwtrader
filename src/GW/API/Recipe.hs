module GW.API.Recipe where

import ClassyPrelude

import GW.API.DbService
import Data.Aeson

data Ingredient =
  Ingredient
  { ingredientItemId :: Int
  , ingredientCount :: Int
  } deriving (Show)

instance FromJSON Ingredient where
  parseJSON (Object v) =
    Ingredient <$>
    (v .:  "item_id")  <*>
    (v .:  "count")

data Recipe =
  RecipeService
  | Recipe
  { recipeId   :: Int
  , recipeType :: String
  , recipeOutputItemId :: Int
  , recipeOutputItemCount :: Int
  , recipeTimeToCraftMS :: Int
  , recipeDiciplines :: [String]
  , recipeMinRating :: Int
  , recipeFlags :: [String]
  , recipeIngredients :: [Ingredient]
  } deriving (Show)

instance FromJSON Recipe where
  parseJSON (Object v) =
    Recipe <$>
    (v .:  "id")  <*>
    (v .:  "type")  <*>
    (v .:  "output_item_id")    <*>
    (v .:  "output_item_count")  <*>
    (v .:  "time_to_craft_ms")    <*>
    (v .:  "disciplines")  <*>
    (v .:  "min_rating")    <*>
    (v .:  "flags")  <*>
    (v .:  "ingredients")

instance ApiDbService Recipe where
  serviceName _ = "recipes"
