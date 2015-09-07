module Item where

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Network.HTTP.Conduit

data Item =
  Item
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

buildItemList :: IO [Item]
buildItemList = do
  items <- downloadItems
  mapM downloadItem (take 1000 items)

downloadItems :: IO [Int]
downloadItems = do
  mabyeItems <- decode <$> downloadItemsJson
  case mabyeItems of
    Nothing    -> error "Cannot parse item list."
    Just items -> return items

downloadItem :: Int -> IO Item
downloadItem n = do
  eitherItem <- eitherDecode <$> downloadItemJson n
  case eitherItem of
    Left  e -> error $ "Cannot parse item " ++ show n ++ ". Error: " ++ e
    Right i -> return i

downloadItemsJson :: IO ByteString
downloadItemsJson =
  simpleHttp "https://api.guildwars2.com/v2/items"

downloadItemJson :: Int -> IO ByteString
downloadItemJson n =
  let url = "https://api.guildwars2.com/v2/items/" ++ show n
  in simpleHttp url
