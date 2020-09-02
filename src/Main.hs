module Main where


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



main :: IO ()
main = do
  text <- getContents
  putStrLn text
