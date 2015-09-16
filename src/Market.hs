module Market where

import ClassyPrelude

import GW.API.DbService
import GW.API.Item
import GW.API.Recipe
import GW.API.Commerce

data MarketItem =
  MarketItem
    { marketItemId :: Int
    , marketItemName :: String
    , marketItemMaterials :: [(Int, Int)] -- (Id, Amount)
    , marketItemSellPrice :: [Int]
    , marketItemBuyPrice :: [Int]
    , marketItemCraftingCost :: Int
    , marketItemCraftingRestriction :: [Int]
    }

syncMarket :: IO ()
syncMarket = do
  syncServiceDb ItemService
  syncServiceDb RecipeService
  freshSyncServiceDb CommerceService

  buildMarket

buildMarket :: IO ()
buildMarket = do
  putStrLn "Building market..."
  putStrLn "...market built."
