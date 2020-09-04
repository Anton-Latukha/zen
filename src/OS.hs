-- * Pragmas

-- * Module
-- This module is to do OS detection, somehow I've not seen other modules that rely on runtime information
module OS where

-- ** Imports
import qualified System.Info as Inf
import qualified System.Posix.Unistd as PUn
import qualified System.Posix.Env as Env
import Control.Exception

-- ** Development debug
debugOut :: Show a => a -> IO ()
debugOut = print

-- ** main function
osDetection :: IO ()
osDetection = do
  debugOut Inf.os
  isPosix <- try (evaluate getPosixVer) :: IO (Either SomeException (IO Integer))
  case isPosix of
    Left ex -> print $ "Exception:" <> show ex
    Right ioPosixVer -> do
      posixVer <- ioPosixVer
      debugOut "Is the POSIX system."
      debugOut "POSIX version:"
      debugOut posixVer
  debugEnv <- Env.getEnvironment
  debugOut debugEnv

  where
   getPosixVer = PUn.getSysVar PUn.PosixVersion
