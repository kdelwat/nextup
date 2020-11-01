module CLI (parseCLI) where

import Commands
import Options.Applicative

addP = Add <$> mTypeArg <*> nameArg <*> artistArg
  where
    mTypeArg = strArgument (metavar "MEDIA_TYPE")
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
