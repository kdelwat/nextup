{-# LANGUAGE OverloadedStrings #-}

module Database (load, save, nextId, recordsOfFormat, Database (..), MediaItem (..), Format (..), Rating (..)) where

import Control.Monad (mzero)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import Text.Printf (printf)

-- https://stackoverflow.com/a/22793986
packStr :: String -> B.ByteString
packStr = encodeUtf8 . T.pack

newtype Rating = Rating (Maybe Int)

instance Show Rating where
  show (Rating r) =
    case r of
      Just n -> show n
      Nothing -> "?"

data Format = Book | Album | Film
  deriving (Show, Read, Eq)

data MediaItem = MediaItem
  { itemId :: !Int,
    format :: !Format,
    name :: !String,
    artist :: !String,
    rating :: !Rating
  }

instance Show MediaItem where
  show m = printf "(%i) %s - %s (%s) [%s]" (itemId m) (name m) (artist m) (show (rating m)) (show (format m))

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
  parseField f = fail ("Not a media format: " ++ show f)

instance ToField Format where
  toField Book = "book"
  toField Album = "album"
  toField Film = "film"

instance FromRecord MediaItem where
  parseRecord v
    | length v == 5 = MediaItem <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4
    | otherwise = mzero

instance ToRecord MediaItem where
  toRecord (MediaItem id' format' name' artist' ranking') =
    record
      [toField id', toField format', toField name', toField artist', toField ranking']

data Database = Database
  { records :: V.Vector MediaItem,
    filename :: String
  }

parse :: String -> BL.ByteString -> Either String Database
parse fname s =
  case decode NoHeader s :: Either String (V.Vector MediaItem) of
    Left err -> Left err
    Right v -> Right (Database {records = v, filename = fname})

load :: String -> IO (Either String Database)
load fname = do
  csvData <-
    BL.readFile
      fname
  return (parse fname csvData)

save :: Database -> IO ()
save db = do
  let output = encode ((V.toList . records) db)
  BL.writeFile (filename db) output

nextId :: Database -> Int
nextId =
  ((+) 1) . maximum . V.toList . V.map itemId . records

recordsOfFormat :: Format -> Database -> V.Vector MediaItem
recordsOfFormat mediaFormat =
  V.filter (\i -> (format i) == mediaFormat) . records
