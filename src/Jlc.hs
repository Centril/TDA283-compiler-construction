module Main where

import System.IO ( stdin, hGetContents, hPutStrLn, stderr )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import Javalette.Lex
import Javalette.Par
import Javalette.Skel
import Javalette.Print
import Javalette.Abs
import Javalette.ErrM

import Frontend.TypeCheck

handleArgs :: [String] -> IO String
handleArgs [] = getContents
handleArgs (f:_) = readFile f

parserPhase :: String -> IO ()
parserPhase s = case pProgram (myLexer s) of
  Bad err  -> do
    putStrLn "SYNTAX ERROR"
    putStrLn err
    exitFailure
  Ok tree -> do
    putStrLn ""
    print tree
    putStrLn ""
    typeCheckPhase tree

typeCheckPhase :: Program -> IO ()
typeCheckPhase p = case typeCheck p of
  Bad err -> do
    putStrLn "TYPE ERROR"
    putStrLn err
    exitFailure
  Ok _ -> putStrLn "OK"

main :: IO ()
main = getArgs >>= handleArgs >>= parserPhase
