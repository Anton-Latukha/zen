module Main where

import Data.Version (showVersion)
import qualified Paths_zen as Package

main :: IO ()
main = putStrLn (showVersion Package.version)
