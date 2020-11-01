module Commands (Command (..), run) where

import Database

data Command
  = Add String String String
  | Stats

runStats :: Database -> (Database, String)
runStats old = (old, "Stats")

runAdd :: Database -> String -> String -> String -> (Database, String)
runAdd old mType name artist =
  (old, "Added: " ++ mType ++ " " ++ name ++ "  " ++ artist)

run :: Command -> Database -> (Database, String)
run cmd old =
  case cmd of
    Stats ->
      runStats old
    Add mType name artist -> runAdd old mType name artist
