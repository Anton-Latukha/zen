module Main where

import Options.Applicative

data Opts
  = Opts
  {
    optLogFile :: String
  }

{- -- Optparse version

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import qualified Paths_zen as Package

data Command
  = Command
  {
    version :: Bool
  }

baseOpt :: Parser Command

versionOpt :: Parser (a -> a)
versionOpt = infoOption (showVersion Package.version) (long "version" <> help "Released version")

opts :: Parser Command
opts = baseOpt <**> helper <**> versionOpt

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (greet <**> helper <**> versionOpt) mempty
    greet :: Command -> IO ()
    greet (Command False) = putStrLn $ "Hello!"
    greet _ = return ()

-}

logFile = "/tmp/tmp.log"

main :: IO ()
main = do
  opts <- execParser optsParser
  text <- getContents
  print text
  appendFile (optLogFile opts) text

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
