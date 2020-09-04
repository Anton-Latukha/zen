module Main where

import Options.Applicative

data Opts
  = Opts
  { logFile :: String
  }

{- -- Optparse version

import qualified Paths_zen as Package

data Command
  = Command
  {
    version :: Bool
  }

versionOpt :: Parser (a -> a)
versionOpt = infoOption (showVersion Package.version) (long "version" <> help "Released version")
-}

logFile = "/tmp/tmp.log"

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
      (helper <*> versionOption <*> programOptions)
      (fullDesc <> progDesc "Zen. Attend to what is important." <>
        header "zen - silence/redirect stdout outputs to get only important information.")

  versionOption :: Parser (a -> a)
  versionOption = infoOption "0.0" (long "version" <> help "Show version")

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
