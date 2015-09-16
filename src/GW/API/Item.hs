module GW.API.Item where

import ClassyPrelude

import GW.API.DbService
import Data.Aeson

data Item =
  ItemService
  | Item
  { itemId            :: Int
  , itemName          :: String
  , itemIcon          :: String
  , itemDescription   :: Maybe String
  , itemtype      :: String
  , itemRarity        :: String
  , itemLevel         :: Int
  , itemVendorValue  :: Int
  , itemDefaultSkin  :: Maybe Int
  , itemFlags         :: [String]
  , itemGameTypes    :: [String]
  , itemRestrictions  :: [String]
  } deriving (Show)

instance FromJSON Item where
  parseJSON (Object v) =
    Item <$>
    (v .:  "id")            <*>
    (v .:  "name")          <*>
    (v .:  "icon")          <*>
    (v .:? "description")   <*>
    (v .:  "type")          <*>
    (v .:  "rarity")        <*>
    (v .:  "level")         <*>
    (v .:  "vendor_value")  <*>
    (v .:? "default_skin")  <*>
    (v .:  "flags")         <*>
    (v .:  "game_types")    <*>
    (v .:  "restrictions")

instance ApiDbService Item where
  serviceName _ = "items"
