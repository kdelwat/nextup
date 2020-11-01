module Commands (Command (..), run) where

import qualified Data.Vector as V
import Database (Database, Format, MediaItem (MediaItem, itemId, rating), Rating (Rating), nextId, records, recordsOfFormat)

data Command
  = Add Format String String
  | Next Format
  | List Format
  | Rate Int Int
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
  let newItem = MediaItem (nextId old) mediaFormat name artist (Rating (Nothing))
   in (old {records = V.snoc (records old) newItem}, "[Added]\n" ++ show newItem)

runNext :: Database -> Format -> (Database, String)
runNext old mediaFormat =
  let next = safeHead . recordsOfFormat mediaFormat
   in case next old of
        Just h -> (old, show h)
        Nothing -> (old, "No unrated items")

runList :: Database -> Format -> (Database, String)
runList old mediaFormat =
  (old, (unlines . fmap show . V.toList . recordsOfFormat mediaFormat) old)

runRate :: Database -> Int -> Int -> (Database, String)
runRate old target newRating =
  let new = old {records = V.map (\i -> if itemId i == target then i {rating = Rating (Just newRating)} else i) (records old)}
   in (new, "Added rating")

run :: Command -> Database -> (Database, String)
run cmd old = case cmd of
  Stats ->
    runStats old
  Add mediaFormat name artist -> runAdd old mediaFormat name artist
  Next mediaFormat -> runNext old mediaFormat
  Rate target newRating -> runRate old target newRating
  List mediaFormat -> runList old mediaFormat
