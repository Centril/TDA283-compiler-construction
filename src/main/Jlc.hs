module Main where

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  hPutStrLn stderr "ERROR"
  exitFailure
