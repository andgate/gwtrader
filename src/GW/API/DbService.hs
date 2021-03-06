module GW.API.DbService where

import ClassyPrelude

import Control.Concurrent (threadDelay)
import Control.Concurrent.ParallelIO
import Data.Aeson
import Network.Connection
import Network.HTTP.Conduit
import System.Directory
import System.ProgressBar

import System.IO ( hSetBuffering, BufferMode(NoBuffering) )

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

  freshSyncServiceDb :: s -> IO ()
  freshSyncServiceDb srv = do
    newServiceDb srv
    fetchServiceDb srv

  syncServiceDb :: s -> IO ()
  syncServiceDb srv = do
    establishServiceDb srv
    fetchMissingEntries srv

  fetchServiceDb :: s -> IO ()
  fetchServiceDb srv = do
    index <- grabIndex srv
    tryFetchAllServiceEntries srv index 3

  fetchMissingEntries :: s -> IO ()
  fetchMissingEntries srv = do
    index <- grabIndex srv
    missing <- findMissingEntries srv index
    tryFetchAllServiceEntries srv missing 3

  grabIndex :: s -> IO [Int]
  grabIndex srv = do
    putStrLn $ "Grabbing " ++ pack (serviceName srv) ++ " index..."
    fetchServiceIndex srv
    loadServiceIndex srv

  findMissingEntries :: s -> [Int] -> IO [Int]
  findMissingEntries srv =
    filterM ((not <$>) . serviceEntryExists srv)

  newServiceDb :: s -> IO ()
  newServiceDb srv = do
    removeServiceDb srv
    establishServiceDb srv

  establishServiceDb :: s -> IO ()
  establishServiceDb srv =
    createDirectoryIfMissing True (apiServiceDb srv)

  removeServiceDb :: s -> IO ()
  removeServiceDb srv =
    doesDirectoryExist (apiServiceDb srv)
    >>= (`when` removeDirectoryRecursive (apiServiceDb srv))


  downloadServiceIndex :: s -> IO LByteString
  downloadServiceIndex srv =
    simpleHttp (apiServiceIndexURL srv)

  saveServiceIndex :: s -> LByteString -> IO ()
  saveServiceIndex srv =
    writeFile (apiServiceIndex srv)

  fetchServiceIndex :: s -> IO ()
  fetchServiceIndex srv =
    downloadServiceIndex srv >>= saveServiceIndex srv

  loadServiceIndex :: s -> IO [Int]
  loadServiceIndex srv =
    readFile (apiServiceIndex srv)
    >>= either
          (\e -> error $ "Cannot parse " ++ serviceName srv ++ " index. Error: " ++ e)
          return
        . eitherDecode


  downloadServiceEntry :: s -> Manager -> Int -> IO LByteString
  downloadServiceEntry srv manager n = do
    request <- parseUrl (apiServiceEntryURL srv n)
    responseBody <$> httpLbs request manager

  saveServiceEntry :: s -> Int -> LByteString -> IO ()
  saveServiceEntry srv n =
    writeFile (apiServiceEntry srv n)

  fetchServiceEntry :: s -> Manager -> Int -> IO ()
  fetchServiceEntry srv manager n =
    downloadServiceEntry srv manager n
      >>= saveServiceEntry srv n

  tryFetchAllServiceEntries :: s -> [Int] -> Int -> IO ()
  tryFetchAllServiceEntries srv index tries
    | tries == 0 =
        putStrLn $ "\nUnable to download entries: " ++ tshow index
                 ++ "\nWARNING: Some "
                 ++ pack (serviceName srv)
                 ++ " entries were not downloaded. This may cause problems."
    | otherwise = do
        failed <- lefts <$> fetchServiceEntries srv index
        case failed of
          [] ->
            putStrLn $ pack (serviceName srv) ++ " database is up-to-date."
          _  -> do
            putStrLn $ "Some entries failed to download:\n" ++ tshow failed
                     ++ "\n\nTrying again in 5 seconds. "
                     ++ tshow tries ++ " tries left."
            threadDelay 5000000
            tryFetchAllServiceEntries srv failed (tries-1)

  fetchServiceEntries :: s -> [Int] -> IO [Either Int Int]
  fetchServiceEntries srv index = do
    let total = fromIntegral $ length index
        header = "Fetching " ++ serviceName srv
        headerLength = fromIntegral $ length header
        settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing

    hSetBuffering stdout NoBuffering
    (progRef, _) <- startProgress (msg header)
                                  exact -- display the exact total
                                  (40 + headerLength)    -- message bar width in characters
                                  total

    manager <- newManager settings
    results <- parallel $
      map (\n ->
            catch (do
                    fetchServiceEntry srv manager n
                    incProgress progRef 1
                    return $ Right n
                  )
                  (\e -> do
                    let err = show (e :: SomeException)
                    return $ Left n
                  )
          )
          index

    putStrLn ""
    return results

  loadServiceEntry :: s -> Int -> IO s
  loadServiceEntry srv n =
    readFile (apiServiceEntry srv n)
    >>= either
          (\e -> error $ "Cannot parse " ++ serviceName srv ++ " service entry " ++ show n ++ ".\nError: " ++ e)
          return
        . eitherDecode

  serviceEntryExists :: s -> Int -> IO Bool
  serviceEntryExists srv =
    doesFileExist . apiServiceEntry srv
