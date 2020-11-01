module Commands (Command (..), run) where

import qualified Data.Vector as V
import Database

data Command
  = Add Format String String
  | Stats

runStats :: Database -> (Database, String)
runStats old =
  let nItems = V.length (records old)
   in (old, "Total items: " ++ show nItems)

runAdd :: Database -> Format -> String -> String -> (Database, String)
runAdd old mType name artist =
  (old, "Added: " ++ show mType ++ " " ++ name ++ "  " ++ artist)

run :: Command -> Database -> (Database, String)
run cmd old =
  case cmd of
    Stats ->
      runStats old
    Add format name artist -> runAdd old format name artist
