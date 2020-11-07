{-# LANGUAGE OverloadedStrings #-}

module Main where

import CLI
import Commands
import Control.Monad (when)
import Data.Ini
import Data.Text as T
import qualified Database as DB
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Exit
import System.FilePath (joinPath)

defaultConfig :: String
defaultConfig = "[DATABASE]\npath: nextup_db.csv\n"

getConfigFilePath :: IO String
getConfigFilePath = do
  configDir <- getXdgDirectory XdgConfig "nextup"
  createDirectoryIfMissing True configDir
  return (joinPath [configDir, "nextup.ini"])

getConfig :: IO (Either String Ini)
getConfig = do
  configPath <- getConfigFilePath
  configExists <- doesFileExist configPath
  when (not configExists) (writeFile configPath defaultConfig)
  readIniFile configPath

exit' :: String -> IO ()
exit' message = do
  putStrLn message
  exitFailure

main :: IO ()
main = do
  options <- parseCLI
  configRes <- getConfig
  case configRes of
    Left err -> exit' err
    Right config -> do
      case lookupValue "DATABASE" "path" config of
        Left err -> exit' err
        Right dbPath -> do
          dbRes <- DB.load (T.unpack dbPath)
          case dbRes of
            Left err -> exit' err
            Right old ->
              let (new, output) = run options old
               in sequence_ [putStrLn output, DB.save new]
