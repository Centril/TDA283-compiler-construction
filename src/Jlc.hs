module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure )

import Javalette.Abs

import Frontend.ParseLex
import Frontend.Types hiding (err)
import Frontend.Typecheck2

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

tc prog = runEval (typeCheck prog) initialEnv

typeCheckPhase :: Program -> IO ()
typeCheckPhase p = case result of
    Left err -> do
        errLn "TYPE ERROR"
        putStrLn err
        putStrLn $ prettify $ show logs
        exitFailure
    Right (ast, env) -> do
        putStrLn $ prettify $ show logs
        putStrLn $ prettify $ show ast
        putStrLn $ prettify $ show env
        errLn "OK"
    where (result, logs) = tc p

main :: IO ()
main = getArgs >>= handleArgs >>= parserPhase