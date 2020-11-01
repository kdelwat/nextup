module Commands (Command (..), run) where

import qualified Data.Vector as V
import Database (Database, Format, MediaItem (MediaItem, format), Rating (Rating), records)

data Command
  = Add Format String String
  | Next Format
  | Stats

safeHead :: V.Vector a -> Maybe a
safeHead v =
  if V.length v > 0 then Just (V.head v) else Nothing

runStats :: Database -> (Database, String)
runStats old =
  let nItems = V.length (records old)
   in (old, "Total items: " ++ show nItems)

runAdd :: Database -> Format -> String -> String -> (Database, String)
runAdd old mediaFormat name artist =
  let newItem = MediaItem mediaFormat name artist (Rating (Nothing))
   in (old {records = V.snoc (records old) newItem}, "[Added]\n" ++ show newItem)

runNext :: Database -> Format -> (Database, String)
runNext old mediaFormat =
  let next = safeHead . V.filter (\i -> (format i) == mediaFormat) . records
   in case next old of
        Just h -> (old, show h)
        Nothing -> (old, "No unrated items")

run :: Command -> Database -> (Database, String)
run cmd old = case cmd of
  Stats ->
    runStats old
  Add mediaFormat name artist -> runAdd old mediaFormat name artist
  Next mediaFormat -> runNext old mediaFormat
