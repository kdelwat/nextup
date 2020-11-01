module Main where

import CLI
import Commands
import qualified Database as DB

main :: IO ()
main = do
  options <- parseCLI
  dbRes <- DB.load "db.csv"
  case dbRes of
    Left err -> putStrLn err
    Right d -> putStrLn (snd (run options d))
