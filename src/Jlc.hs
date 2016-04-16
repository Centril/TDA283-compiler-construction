module Main where

import System.IO ( stdin, hGetContents )
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
import Utils.Terminal

handleArgs :: [String] -> IO String
handleArgs [] = getContents
handleArgs (f:_) = readFile f

handleArgs :: [String] -> IO String
handleArgs [] = getContents
handleArgs (f:_) = readFile f

parserPhase :: String -> IO ()
parserPhase s = case pProgram (myLexer s) of
  Bad err  -> do
    errLn "SYNTAX ERROR"
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
    errLn "TYPE ERROR"
    putStrLn err
    exitFailure
  Ok _ -> errLn "OK"

main :: IO ()
<<<<<<< HEAD
main = getArgs >>= handleArgs >>= parserPhase
=======
main = getArgs >>= handleArgs >>= parserPhase
>>>>>>> Jlc: making use of Utils.Terminal.
