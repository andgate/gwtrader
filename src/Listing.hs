module Listing where

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe
import Data.Word
import Network.HTTP.Conduit


data Listing =
  Listing
  { listings :: Int
  , listingUnitPrice :: Int
  , listingQuantity :: Int
  } deriving (Show)

instance FromJSON Listing where
  parseJSON (Object v) =
    Listing <$>
    (v .:  "listings")    <*>
    (v .:  "unit_price")  <*>
    (v .:  "quantity")

data TPItem =
  TPItem
  { tpItemId :: Int
  , tpItemBuys :: [Listing]
  , tpItemSells :: [Listing]
  } deriving (Show)

parseListing :: Value -> Listing
parseListing v =
  let listingResult = fromJSON v
  in case listingResult of
    Success l -> l
    Error   e -> error $ "Could not parse listing. \nError: " ++ e

instance FromJSON TPItem where
  parseJSON (Object v) =
    TPItem <$>
    (v .:  "id")          <*>
    (v .: "buys")  <*>
    (v .: "sells")

buildTPItemList :: IO [TPItem]
buildTPItemList = do
  items <- downloadTPItems
  mapM downloadTPItem (take 10 items)
  
downloadTPItems :: IO [Int]
downloadTPItems =
  fromJust . decode <$> downloadTPItemsJson

downloadTPItemsJson :: IO ByteString
downloadTPItemsJson =
  simpleHttp "https://api.guildwars2.com/v2/commerce/listings"

downloadTPItem :: Int -> IO TPItem
downloadTPItem n = do
  eitherTPItem <- eitherDecode <$> downloadTPItemJson n
  case eitherTPItem of
    Right tpi -> return tpi
    Left  e   -> error $ "Cannot parse trading post item " ++ show n ++ ". Error: " ++ e

downloadTPItemJson :: Int -> IO ByteString
downloadTPItemJson n =
  let url = "https://api.guildwars2.com/v2/commerce/listings/" ++ show n
  in simpleHttp url
