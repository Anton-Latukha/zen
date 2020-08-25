module Main where

import Data.Version (showVersion)
import Paths_zen (version)

main :: IO ()
main = putStrLn (showVersion version)
