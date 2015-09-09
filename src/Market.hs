module Market where

import GW.API.DbService
import GW.API.Item
import GW.API.Recipe
import GW.API.Commerce

data MarketItem =
  MarketItem
    { marketItemId :: Int
    , marketItemName :: String
    }

syncMarket :: IO ()
syncMarket = do
  syncServiceDb ItemService
  syncServiceDb RecipeService
  freshSyncServiceDb CommerceService

  --buildMarket

buildMarket :: IO ()
buildMarket = undefined
