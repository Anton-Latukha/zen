module Main where

import Options.Applicative
import qualified Paths_zen as Package
import Data.Version

data Opts
  = Opts
  { logFile :: String
  }


main :: IO ()
main = do
  opts <- execParser optsParser
  text <- getContents
  print text
  appendFile (logFile opts) text

 where
  optsParser :: ParserInfo Opts
  optsParser =
    info
      options
      introduction
     where
      options = helper <*> versionOpt <*> programOptions
      introduction =
        fullDesc
        <> progDesc "Zen. Attend to what is important."
        <> header "zen - silence/redirect stdout outputs to get only important information."

  versionOpt :: Parser (a -> a)
  versionOpt =
    infoOption
      versionStr optDesc
     where
      versionStr = showVersion Package.version
      optDesc = long "version" <> help "Version release"

  logFileOpt :: Parser String
  logFileOpt = strOption $
    long "file"
    <> short 'f'
    <> metavar "LOGFILE"
    <> help "Direct log into a file"

  programOptions :: Parser Opts
  programOptions =
    Opts
    <$> logFileOpt
