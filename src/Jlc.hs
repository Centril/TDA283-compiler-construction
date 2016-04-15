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

type Verbosity = Int
type ParseFun a = [Token] -> Err a

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

parseFile :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
parseFile v p s = let ts = myLexer s in case p ts of
             Bad s    -> do putStrLn "\nParse failed...\n"
                            exitFailure
             Ok  tree -> do putStrLn "\nParse successful!\n"
                            putStrV v $ show tree
                            putStrLn ""
                            exitSuccess

main :: IO ()
main = do
  getContents >>= parseFile 2 pProgram
  typeCheck
  hPutStrLn stderr "ERROR"
  exitFailure
