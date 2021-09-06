module Main where

import System.IO (hSetEncoding, utf8, stdout, stderr)
import Jih (jihCLI)


main :: IO ()
main = hSetEncoding stdout utf8 >> hSetEncoding stderr utf8 >> jihCLI
