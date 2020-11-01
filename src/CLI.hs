module CLI (parseCLI) where

import Commands
import Control.Arrow (left)
import Database
import Options.Applicative
import Text.Read (readEither)

formatReader :: ReadM Format
formatReader =
  eitherReader reader
  where
    reader input = left (\_ -> "Invalid media type") $ readEither input

formatArg :: Parser Format
formatArg = argument formatReader (metavar "MEDIA_FORMAT")

intReader :: ReadM Int
intReader =
  eitherReader reader
  where
    reader input = left (\_ -> "Not an integer") $ readEither input

addP :: Parser Command
addP = Add <$> formatArg <*> nameArg <*> artistArg
  where
    nameArg = strArgument (metavar "NAME")
    artistArg = strArgument (metavar "ARTIST")

nextP :: Parser Command
nextP = Next <$> formatArg

listP :: Parser Command
listP = List <$> formatArg

rateP :: Parser Command
rateP = Rate <$> idArg <*> ratingArg
  where
    idArg = argument intReader (metavar "ID")
    ratingArg = argument intReader (metavar "RATING")

commandParser :: ParserInfo Command
commandParser =
  let cmds =
        subparser
          ( command
              "stats"
              ( info (helper <*> pure Stats) (fullDesc <> progDesc "")
              )
              <> command
                "add"
                (info (helper <*> addP) (fullDesc <> progDesc ""))
              <> command "next" (info (helper <*> nextP) (fullDesc <> progDesc "Get the next unrated item"))
              <> command "list" (info (helper <*> listP) (fullDesc <> progDesc "List all items of a particular format"))
              <> command "rate" (info (helper <*> rateP) (fullDesc <> progDesc "Rate an item"))
          )
   in info
        (cmds <**> helper)
        (fullDesc <> progDesc "Manage your media queue")

parseCLI :: IO Command
parseCLI = execParser commandParser
