{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Csv
import Control.Monad (mzero)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

-- https://stackoverflow.com/a/22793986
packStr :: String -> B.ByteString
packStr = encodeUtf8 . T.pack

newtype Rating = Rating (Maybe Int)
    deriving (Show)

instance FromField Rating where
    parseField s = case runParser (parseField s) of
        Left _ -> pure $ Rating (Nothing)
        Right n -> pure $ Rating (Just n)

instance ToField Rating where
    toField (Rating (Just n)) = packStr (show n)
    toField (Rating (Nothing)) = ""

data MediaItem = MediaItem
    { name :: !String,
      artist :: !String,
      rating :: !Rating}
      deriving (Show)

instance FromRecord MediaItem where
    parseRecord v
        | length v == 3 = MediaItem <$> v .! 0 <*> v .! 1 <*> v .! 2
        | otherwise = mzero

instance ToRecord MediaItem where
    toRecord (MediaItem name' artist' ranking') = record
        [toField name', toField artist', toField ranking']

main :: IO ()
main = do
    csvData <- BL.readFile "db.csv"
    case decode NoHeader csvData :: Either String (V.Vector MediaItem) of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ p ->
            putStrLn $ name p ++ " (" ++ artist p ++ ")"


