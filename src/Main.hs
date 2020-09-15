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
import qualified System.Posix.Terminal as Term
import qualified System.Posix.IO as PIO
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
  , wrappedCommand :: Maybe String
  }

-- ** Development debug
debugOut :: Show a => a -> IO ()
debugOut = print

-- ** main function
main :: IO ()
main = do
  opts <- OPA.execParser optsParser
  appName <- Env.getProgName
  isTermStdIn <- ioIsTermStdIn
  Log.withSyslog appName [Log.LogPID] Log.User $ do
    case (isTermStdIn, wrappedCommand opts) of
      (True, Just command) -> undefined
        -- TODO: Log and output to the terminal warn that only one of stdin stream OR wrapped command should be present, and throw an error right after that.
        -- putStrLn command
      (False, Just command) -> undefined
        -- TODO: Construct a shell execution wrapper for the command -> go into the default logging flow
        -- text <- withCreateProcess Proc.shell command
        -- defaultLogFlow text (logFile opts)
      (True, Nothing) -> do
        -- TODO: go into the default logging flow
        text <- getContents
        defaultLogFlow text (logFile opts)
      (False, Nothing) -> undefined
        -- TODO: Log from itself and out to terminal that the launch was vacuos. Determine would tool exit normally (aka `echo`) or with error on no input, as `grep`?

 where

  defaultLogFlow :: Show a => a -> Maybe FilePath -> IO ()
  defaultLogFlow text maybeLogFile = do
    text <- getContents
    send text
    -- If file is provided - also log into a file.
    case maybeLogFile of
      Just path -> appendFile path text
      Nothing -> pure ()

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

  ioIsTermStdIn = Term.queryTerminal PIO.stdInput
