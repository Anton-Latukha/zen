-- * Pragmas

-- * Module
module Debug where

-- ** Imports

-- ** Functions

-- *** Development debug
debugOut :: Show a => a -> IO ()
debugOut = print
