-- * Pragmas

-- * Module
module Main where

-- ** Imports
import qualified Options.Applicative as OPA
import qualified Paths_zen as Pac
import Data.Version as Ver


-- ** Data types
data Opts
  = Opts
  { logFile :: String
  }

-- ** Development debug
debugOut = print


-- ** main function
main :: IO ()
main = do
  opts <- OPA.execParser optsParser
  text <- getContents
  debugOut text
  appendFile (logFile opts) text

 where
  optsParser :: OPA.ParserInfo Opts
  optsParser =
    OPA.info
      options
      introduction
     where
      options = OPA.helper <*> versionOpt <*> programOptions
      introduction =
        OPA.fullDesc
        <> OPA.progDesc "Zen. Attend to what is important."
        <> OPA.header "zen - silence/redirect stdout outputs to get only important information."

  versionOpt :: OPA.Parser (a -> a)
  versionOpt =
    OPA.infoOption
      versionStr optDesc
     where
      versionStr = Ver.showVersion Pac.version
      optDesc = OPA.long "version" <> OPA.help "Version release"

  logFileOpt :: OPA.Parser String
  logFileOpt =
    OPA.strOption $
      OPA.long "file"
      <> OPA.short 'f'
      <> OPA.metavar "LOGFILE"
      <> OPA.help "Direct log into a file"

  programOptions :: OPA.Parser Opts
  programOptions =
    Opts
    <$> logFileOpt
