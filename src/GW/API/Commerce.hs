module GW.API.Commerce where

import GW.API.DbService
import Data.Aeson


data TradeListing =
  TradeListing
    { listings          :: Int
    , listingUnitPrice  :: Int
    , listingQuantity   :: Int
    }
  deriving (Show)

instance FromJSON TradeListing where
  parseJSON (Object v) =
    TradeListing          <$>
    (v .:  "listings")    <*>
    (v .:  "unit_price")  <*>
    (v .:  "quantity")


data Commerce =
  CommerceService
  | CommerceListing
      { tpItemId    :: Int
      , tpItemBuys  :: [TradeListing]
      , tpItemSells :: [TradeListing]
      }
  deriving (Show)

instance FromJSON Commerce where
  parseJSON (Object v) =
    CommerceListing <$>
    (v .: "id")     <*>
    (v .: "buys")   <*>
    (v .: "sells")

instance ApiDbService Commerce where
  serviceName _ = "commerce/listings"
