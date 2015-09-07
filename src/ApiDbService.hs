module ApiDbService where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
import Control.Concurrent.ParallelIO
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Network.Connection
import Network.HTTP.Conduit
import System.Directory
import System.FilePath
import System.ProgressBar

import System.IO ( hSetBuffering, BufferMode(NoBuffering), stdout )

apiURL :: String
apiURL = "https://api.guildwars2.com/v2/"

dbEntryFormat :: FilePath
dbEntryFormat =
  "json"

root :: FilePath
root =
  "cache"

apiDb :: FilePath
apiDb =
  root </> "api"

class FromJSON s => ApiDbService s where
  serviceName :: s -> String

  apiServiceIndexURL :: s -> FilePath
  apiServiceIndexURL srv =
    apiURL </> serviceName srv

  apiServiceEntryURL :: s -> Int -> FilePath
  apiServiceEntryURL srv n =
    apiServiceIndexURL srv </> show n

  apiServiceDb :: s -> FilePath
  apiServiceDb srv =
    apiDb </> serviceName srv

  apiServiceIndex :: s -> FilePath
  apiServiceIndex srv =
    apiServiceDb srv </> "index" <.> dbEntryFormat

  apiServiceEntry :: s -> Int -> FilePath
  apiServiceEntry srv n =
    apiServiceDb srv </> show n <.> dbEntryFormat

  updateServiceDb :: s -> IO ()
  updateServiceDb srv = do
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings

    newServiceDb srv

    putStrLn $ "Grabbing " ++ serviceName srv ++ " index..."
    fetchServiceIndex srv manager
    index <- loadServiceIndex srv

    let total = fromIntegral $ length index
        header = "Fetching " ++ serviceName srv
        headerLength = fromIntegral $ length header

    hSetBuffering stdout NoBuffering
    (progRef, _) <- startProgress (msg header)
                                  exact -- display the exact total
                                  (40 + headerLength)    -- message bar width in characters
                                  total

    parallel_ $
      map (\n -> do
            fetchServiceEntry srv manager n
            incProgress progRef 1)
          index

    return ()

  newServiceDb :: s -> IO ()
  newServiceDb srv =
    clearServiceDb srv
    >> createDirectoryIfMissing True (apiServiceDb srv)

  clearServiceDb :: s -> IO ()
  clearServiceDb srv =
    doesDirectoryExist (apiServiceDb srv)
    >>= (`when` removeDirectoryRecursive (apiServiceDb srv))


  downloadServiceIndex :: s -> Manager -> IO ByteString
  downloadServiceIndex srv manager =
    tryDownload manager (apiServiceIndexURL srv)

  saveServiceIndex :: s -> ByteString -> IO ()
  saveServiceIndex srv =
    BS.writeFile (apiServiceIndex srv)

  fetchServiceIndex :: s -> Manager -> IO ()
  fetchServiceIndex srv manager =
    downloadServiceIndex srv manager >>= saveServiceIndex srv

  loadServiceIndex :: s -> IO [Int]
  loadServiceIndex srv =
    BS.readFile (apiServiceIndex srv)
    >>= either
          (\e -> error $ "Cannot parse " ++ serviceName srv ++ " index. Error: " ++ e)
          return
        . eitherDecode


  downloadServiceEntry :: s -> Manager -> Int -> IO ByteString
  downloadServiceEntry srv manager =
    tryDownload manager . apiServiceEntryURL srv

  saveServiceEntry :: s -> Int -> ByteString -> IO ()
  saveServiceEntry srv n =
    BS.writeFile (apiServiceEntry srv n)

  fetchServiceEntry :: s -> Manager -> Int -> IO ()
  fetchServiceEntry srv manager n =
    downloadServiceEntry srv manager n >>= saveServiceEntry srv n

  loadServiceEntry :: s -> Int -> IO s
  loadServiceEntry srv n =
    BS.readFile (apiServiceEntry srv n)
    >>= either
          (\e -> error $ "Cannot parse " ++ serviceName srv ++ " service entry " ++ show n ++ ".\nError: " ++ e)
          return
        . eitherDecode

tryDownload :: Manager -> String -> IO ByteString
tryDownload manager url = do
  request <- parseUrl url
  catch (responseBody <$> httpLbs request manager)
        (\e -> do
          let err = show (e :: SomeException)
          putStrLn "Download failed, retrying in 5 seconds"
          threadDelay 3000000
          tryDownload manager url
        )
