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

addP = Add <$> formatArg <*> nameArg <*> artistArg
  where
    formatArg = argument formatReader (metavar "MEDIA_FORMAT")
    nameArg = strArgument (metavar "NAME")
    artistArg = strArgument (metavar "ARTIST")

commandParser :: ParserInfo Command
commandParser =
  let cmds =
        subparser
          ( command
              "stats"
              ( info (helper <*> pure Stats) (fullDesc <> progDesc "")
              )
              <> command "add" (info (helper <*> addP) (fullDesc <> progDesc ""))
          )
   in info
        (cmds <**> helper)
        (fullDesc <> progDesc "Manage your media queue")

parseCLI :: IO Command
parseCLI = execParser commandParser
