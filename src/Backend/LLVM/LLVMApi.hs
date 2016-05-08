{- Javalette Compiler, a simple C like language.
 - Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -
 - This program is free software; you can redistribute it and/or
 - modify it under the terms of the GNU General Public License
 - as published by the Free Software Foundation; either version 2
 - of the License, or (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program; if not, write to the Free Software
 - Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 -}

{-|
Module      : Backend.LLVM.LLVMApi
Description : The interface to external programs of the Javalette backend.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

The interface to external programs of the Javalette backend.
-}

module Backend.LLVM.LLVMApi where

import Control.Exception
import Control.Monad
import System.Exit
import System.Process

import Backend.LLVM.LLVMAst
import Backend.LLVM.Print
import Backend.LLVM.JRuntime

--------------------------------------------------------------------------------
-- LLVM phases with error handling:
--------------------------------------------------------------------------------

buildExecutable :: LLVMCode -> FilePath -> String -> IO ()
buildExecutable code path name = do
    let runtime = concatPath path "runtime"
    let main = concatPath path name
    let out = concatPath path "out"
    runLLVMWriter code main
    runLLVMWriter runtimeLLVM runtime
    runLLVMAssembler main
    runLLVMAssembler runtime
    runLLVMLinker [main, runtime] out
    runLLVMCompiler out (concatPath path "a")
    return ()

runLLVMWriter :: String -> FilePath -> IO ()
runLLVMWriter ast file = do
    result <- execLLVMWriter ast $ llFile file
    case result of
        Left  e -> print e
        Right _ -> return ()

runLLVMAssembler :: FilePath -> IO ()
runLLVMAssembler file = runLLVMProg "ASSEMBLER" "llvm-as"
    [llFile file]

runLLVMLinker :: [FilePath]-> FilePath -> IO ()
runLLVMLinker files out = runLLVMProg "LINKER" "llvm-link" $
    fmap bcFile files ++ ["-o=" ++ bcFile out]

runLLVMCompiler :: FilePath -> FilePath -> IO ()
runLLVMCompiler file out = runLLVMProg "COMPILER" "llvm-gcc"
    [bcFile file, "-o", outFile out]

runLLVMProg :: String -> String -> [String] -> IO ()
runLLVMProg phase cmd args = do
    (c,_,e) <- execLLVMProg cmd args
    when (c /= ExitSuccess) $ llvmErr phase e

--------------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------------

execLLVMWriter :: String -> FilePath -> IO (Either SomeException ())
execLLVMWriter code file = try $ writeFile file code

execLLVMProg :: String -> [String] -> IO (ExitCode, String, String)
execLLVMProg cmd args = readProcessWithExitCode cmd args []

llvmErr :: String -> FilePath -> IO()
llvmErr s ex = die $ unwords ["LLVM", s, "ERROR", ex]

llFile, bcFile, outFile :: FilePath -> String
llFile = flip addExt ".ll"
bcFile = flip addExt ".bc"
outFile = flip addExt ".out"

addExt :: FilePath -> String -> String
addExt file ext = file ++ ext

concatPath :: FilePath -> String -> FilePath
concatPath p1 p2 = p1 ++ p2