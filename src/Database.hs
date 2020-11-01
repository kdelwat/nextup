{-# LANGUAGE OverloadedStrings #-}

module Database (load, save, Database (..), MediaItem (..), Format (..), Rating (..)) where

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
  { format :: !Format,
    name :: !String,
    artist :: !String,
    rating :: !Rating
  }

instance Show MediaItem where
  show m = printf "[%s] %s - %s (%s)" (show (format m)) (name m) (artist m) (show (rating m))

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
    | length v == 4 = MediaItem <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3
    | otherwise = mzero

instance ToRecord MediaItem where
  toRecord (MediaItem format' name' artist' ranking') =
    record
      [toField format', toField name', toField artist', toField ranking']

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
