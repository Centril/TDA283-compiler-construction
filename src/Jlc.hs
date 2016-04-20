module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure )

import Javalette.Abs

import Frontend.ParseLex
import Frontend.Types hiding (err)
import Frontend.Typecheck2

import Utils.Terminal
import Utils.Debug

output :: Show a => a -> IO ()
output = putStrLn . prettify . show

handleArgs :: [String] -> IO String
handleArgs [] = getContents
handleArgs (f:_) = readFile f

parserPhase :: String -> IO ()
parserPhase code = case parseProgram code of
    Left err  -> do
        errLn "SYNTAX ERROR"
        putStrLn err
        exitFailure
    Right tree -> do
        putStrLn ""
        output tree
        typeCheckPhase tree

typeCheck' :: Program -> EvalResult Program
typeCheck' prog = runEval (typeCheck prog) initialEnv

typeCheckPhase :: Program -> IO ()
typeCheckPhase prog = case result of
    Left err -> do
        errLn "TYPE ERROR"
        putStrLn err
        output logs
        exitFailure
    Right (ast, env) -> do
        output logs
        output ast
        output env
        errLn "OK"
    where (result, logs) = typeCheck' prog

main :: IO ()
main = getArgs >>= handleArgs >>= parserPhase