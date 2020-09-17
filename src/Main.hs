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
import qualified System.IO as IO
import Log
  ( send
  , sendNotice
  , sendWarn
  , sendError
  , sendCritical
  , sendAlert
  , sendEmergency
  )
import Debug

-- ** Data types
data Opts
  = Opts
  { logFile :: Maybe String
  , wrappedCommand :: Maybe String
  }


-- ** main function
main :: IO ()
main = do
  opts <- OPA.execParser optsParser
  appName <- Env.getProgName
  stdinIsInteractiveTerm <- ioStdinIsInteractiveTerm
  Log.withSyslog appName [Log.LogPID] Log.User $
    case (stdinIsInteractiveTerm, wrappedCommand opts) of
      (True, Just command) -> do
        Proc.withCreateProcess (Proc.shell command)
          { Proc.std_in = Proc.CreatePipe
          , Proc.std_out = Proc.CreatePipe
          , Proc.std_err = Proc.CreatePipe
          }
          (\
            _ -- maybeChildInHandle
            maybeChildOutHandle
            _ -- maybeChildErrHandle
            _ -- processHandle
              -> do
                case maybeChildOutHandle of
                  Just childOutHandle -> do
                    text <- IO.hGetContents childOutHandle
                    defaultLogFlow text (logFile opts)
                  Nothing ->
                  -- TODO: Report that handler not returned, ?error out?
                    undefined
            -- TODO: If something came down the childOutErr pipe, log it and throw an error with it
              )
      (True, Nothing) -> do
        undefined
        -- TODO: Log from itself and out to terminal that the launch was vacuos. Determine would tool exit normally (aka `echo`) or with error on no input, as `grep`?
      (False, Just command) -> do
        -- TODO: Log and output to the terminal warn that only one of stdin stream OR wrapped command should be present, and throw an error right after that.
        undefined
      (False, Nothing) -> do
        -- TODO: go into the default logging flow
        -- text <- Term.drainOutput PIO.stdInput
        text <- getContents
        defaultLogFlow text (logFile opts)

 where

  defaultLogFlow :: Show a => a -> Maybe FilePath -> IO ()
  defaultLogFlow text maybeLogFile = do
    send text
    -- If file is provided - also log into a file.
    case maybeLogFile of
      Just path -> appendFile path (show text)
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

  ioStdinIsInteractiveTerm = Term.queryTerminal PIO.stdInput
