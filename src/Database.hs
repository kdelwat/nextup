{-# LANGUAGE OverloadedStrings #-}

module Database (load, Database (..), MediaItem (..), Format (..)) where

import Control.Monad (mzero)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V

-- https://stackoverflow.com/a/22793986
packStr :: String -> B.ByteString
packStr = encodeUtf8 . T.pack

newtype Rating = Rating (Maybe Int)
  deriving (Show)

data Format = Book | Album | Film
  deriving (Show, Read)

data MediaItem = MediaItem
  { format :: !Format,
    name :: !String,
    artist :: !String,
    rating :: !Rating
  }
  deriving (Show)

instance FromField Rating where
  parseField s = case runParser (parseField s) of
    Left _ -> pure $ Rating (Nothing)
    Right n -> pure $ Rating (Just n)

instance ToField Rating where
  toField (Rating (Just n)) = packStr (show n)
  toField (Rating (Nothing)) = ""

instance FromField Format where
  parseField "book" =
    pure Book
  parseField "album" =
    pure Album
  parseField "film" =
    pure Film

instance ToField Format where
  toField Book = "book"
  toField Album = "album"
  toField Film = "film"

instance FromRecord MediaItem where
  parseRecord v
    | length v == 4 = MediaItem <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3
    | otherwise = mzero

instance ToRecord MediaItem where
  toRecord (MediaItem format' name' artist' ranking') =
    record
      [toField format', toField name', toField artist', toField ranking']

data Database = Database
  { records :: V.Vector MediaItem
  }

parse :: BL.ByteString -> Either String Database
parse s =
  case decode NoHeader s :: Either String (V.Vector MediaItem) of
    Left err -> Left err
    Right v -> Right (Database {records = v})

load :: String -> IO (Either String Database)
load filename = do
  csvData <-
    BL.readFile
      filename
  return (parse csvData)
