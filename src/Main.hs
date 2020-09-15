-- * Pragmas

-- * Module
module Main where

-- ** Imports
import qualified Options.Applicative as OPA
import qualified Paths_zen as Pac
import qualified Data.Version as Ver
import qualified System.Posix.Syslog as Log
import qualified System.Environment as Env
import qualified Foreign.C.String as CStr
import qualified System.Process as Proc
import Log
  ( send
  , sendNotice
  , sendWarn
  , sendError
  , sendCritical
  , sendAlert
  , sendEmergency
  )

-- ** Data types
data Opts
  = Opts
  { logFile :: Maybe String
  , wrappedCommand :: String
  }

-- ** Development debug
debugOut :: Show a => a -> IO ()
debugOut = print

-- ** main function
main :: IO ()
main = do
  opts <- OPA.execParser optsParser
  appName <- Env.getProgName
  Log.withSyslog appName [Log.LogPID] Log.User $ do
    sendNotice $ wrappedCommand opts
    text <- getContents
    sendNotice text
    case logFile opts of
      Just path -> appendFile path text
      Nothing -> pure ()

 where

  optsParser :: OPA.ParserInfo Opts
  optsParser =
    OPA.info
      options
      introduction
     where
      options = OPA.helper <*> versionOpt <*> programOptions
      introduction :: OPA.InfoMod a
      introduction =
        OPA.fullDesc
        <> OPA.progDesc "Zen. Attend to what is important."
        <> OPA.header "zen - silence&redirect stdout outputs to get only important information."

  versionOpt :: OPA.Parser (a -> a)
  versionOpt =
    OPA.infoOption
      versionStr optDesc
     where
      versionStr = Ver.showVersion Pac.version
      optDesc = OPA.long "version" <> OPA.help "Version release"

  programOptions :: OPA.Parser Opts
  programOptions =
    Opts
    <$> OPA.optional logFileOpt
    <*> OPA.optional wrappedCommandOpt
   where
    wrappedCommandOpt :: OPA.Parser String
    wrappedCommandOpt =
      OPA.strArgument $
        OPA.metavar "COMMAND"
        <> OPA.help "Command to run inside Zen"

    logFileOpt :: OPA.Parser String
    logFileOpt =
      OPA.strOption $
        OPA.long "file"
        <> OPA.short 'f'
        <> OPA.metavar "LOGFILE"
        <> OPA.help "Direct log into a file"

  maybeCommand = wrappedCommand opts
