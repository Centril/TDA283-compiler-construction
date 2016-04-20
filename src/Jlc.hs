module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure )

import Javalette.Abs

import Frontend.ParseLex
import Frontend.Types hiding (err)
import Frontend.Typecheck2

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
        putStrLn $ unwords ["ERROR!", "[" ++ show phase ++ "]", errMsg except]
        output logs
        errLn $ phaseErr phase
        exitFailure
    Right (val, env) -> do
        putStrLn "COMPILATION SUCCESS!"
        putStrLn "Accumulated logs:"        >> output logs
        putStrLn "Final computed value:"    >> output val
        putStrLn "Final environment value:" >> output env
        errLn "OK"
    where (result, logs) = runCompileComp code

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
    infoln Parser      ["AST after parse:",      show ast1]
    ast2 <- typeCheck ast1
    infoln TypeChecker ["AST after type check:", show ast2]
    return ast2