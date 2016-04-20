module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure )

import Javalette.Abs

import Frontend.ParseLex
import Frontend.Types hiding (err)
import Frontend.TypeCheck

import Utils.Terminal
import Utils.Debug

main :: IO ()
main = getArgs >>= handleArgs >>= compileUnit

handleArgs :: [String] -> IO String
handleArgs [] = getContents
handleArgs (f:_) = readFile f

compileUnit :: String -> IO ()
compileUnit code =
    case  result of
    Left  except  -> do
        let phase = errPhase except
        errLn $ phaseErr phase
        putStrLn $ unwords ["ERROR!", "[" ++ show phase ++ "]", errMsg except]
        output logs
        exitFailure
    Right (val, env) -> do
        putStrLn "COMPILATION SUCCESS!"
        putStrLn "Accumulated logs:"        >> printLogs logs
        putStrLn "Final computed value:"    >> output val
        putStrLn "Final environment value:" >> output env
        errLn "OK"
    where (result, logs) = runCompileComp code

printLogs :: InfoLog -> IO ()
printLogs logs = putStrLn $ unlines $ showLog <$> logs

showLog :: LogItem -> String
showLog (LogItem lvl phase msg) =
    concat ["[", show lvl, "] [", show phase, "]: " ++ prettify msg]

output :: Show a => a -> IO ()
output = putStrLn . prettify . show

phaseErr :: Phase -> String
phaseErr Parser        = "SYNTAX ERROR"
phaseErr TypeChecker   = "TYPE ERROR"
phaseErr ReturnChecker = "TYPE ERROR"

runCompileComp :: String -> EvalResult Program
runCompileComp code = runEval (compileComp code) initialEnv

compileComp :: String -> Eval Program
compileComp code = do
    ast1 <- parseProgram code
    info Parser "AST after parse"
    info Parser $ show ast1
    ast2 <- typeCheck ast1
    info TypeChecker "AST after type check"
    info TypeChecker $ show ast2
    return ast2