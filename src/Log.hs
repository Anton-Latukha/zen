-- * Pragmas

-- * Module
module Log where

-- ** Imports
import qualified System.Posix.Syslog as Log
import qualified Foreign.C.String as CStr

-- ** Functions
sendLvl :: Show a => Log.Priority -> a -> IO ()
sendLvl lvl text = Log.syslog Nothing lvl =<< CStr.newCAStringLen (show text)

send :: Show a => a -> IO ()
send = sendLvl Log.Info

sendNotice :: Show a => a -> IO ()
sendNotice = sendLvl Log.Notice

sendWarn :: Show a => a -> IO ()
sendWarn = sendLvl Log.Warning

sendError :: Show a => a -> IO ()
sendError = sendLvl Log.Error

sendCritical :: Show a => a -> IO ()
sendCritical = sendLvl Log.Critical

sendAlert :: Show a => a -> IO ()
sendAlert = sendLvl Log.Alert

sendEmergency :: Show a => a -> IO ()
sendEmergency = sendLvl Log.Emergency
