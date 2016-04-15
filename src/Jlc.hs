module Main where

import System.IO ( stdin, hGetContents, hPutStrLn, stderr )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Javalette.Lex
import Javalette.Par
import Javalette.Skel
import Javalette.Print
import Javalette.Abs

import Frontend.TypeCheck

main :: IO ()
main = do
  typeCheck
  hPutStrLn stderr "ERROR"
  exitFailure
