module Recipe where

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Network.HTTP.Conduit

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
  Recipe
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


buildRecipeList :: IO [Recipe]
buildRecipeList = do
  recipes <- downloadRecipes
  mapM downloadRecipe (take 10 recipes)

downloadRecipes :: IO [Int]
downloadRecipes = do
  maybeRecipes <- decode <$> downloadRecipesJson
  case maybeRecipes of
    Nothing -> error "Cannot parse recipe list."
    Just r  -> return r

downloadRecipesJson :: IO ByteString
downloadRecipesJson =
  simpleHttp "https://api.guildwars2.com/v2/recipes"

downloadRecipe :: Int -> IO Recipe
downloadRecipe n = do
  eitherRecipe <- eitherDecode <$> downloadRecipeJson n
  case eitherRecipe of
    Left  e -> error $ "Cannot parse recipe " ++ show n ++ ". Error: " ++ e
    Right r -> return r

downloadRecipeJson :: Int -> IO ByteString
downloadRecipeJson n =
  let url = "https://api.guildwars2.com/v2/recipes/" ++ show n
  in simpleHttp url
