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

import Frontend.ParseLex
import Frontend.TypeCheck
import Utils.Terminal
import Utils.Debug

handleArgs :: [String] -> IO String
handleArgs [] = getContents
handleArgs (f:_) = readFile f

parserPhase :: String -> IO ()
parserPhase s = case parseProgram s of
    Left err  -> do
        errLn "SYNTAX ERROR"
        putStrLn err
        exitFailure
    Right tree -> do
        putStrLn ""
        putStrLn $ prettify $ show tree
        typeCheckPhase tree

typeCheckPhase :: Program -> IO ()
typeCheckPhase p = case typeCheck p of
    Left err -> do
        errLn "TYPE ERROR"
        putStrLn err
        exitFailure
    Right env -> do
        putStrLn ""
        putStrLn $ prettify $ show env
        errLn "OK"

main :: IO ()
main = getArgs >>= handleArgs >>= parserPhase