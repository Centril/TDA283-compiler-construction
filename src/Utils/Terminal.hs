module Utils.Terminal where

import System.IO

errStr, errLn :: String -> IO ()
errStr = hPutStr   stderr
errLn  = hPutStrLn stderr

errChar :: Char -> IO ()
errChar = hPutChar stderr

errPrint :: Show a => a -> IO ()
errPrint = hPrint  stderr