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

import System.Exit
import System.FilePath
import System.Process

import Backend.LLVM.LLVMAst
import Backend.LLVM.JRuntime

--------------------------------------------------------------------------------
-- LLVM phases with error handling:
--------------------------------------------------------------------------------

buildMainExec :: LLVMCode -> FilePath -> String -> IO FilePath
buildMainExec code dest name = do
    main <- buildMainBC code dest name
    runt <- buildRuntimeBC dest
    buildExecutable [main, runt] dest

buildMainBC :: LLVMCode -> FilePath -> String -> IO FilePath
buildMainBC code dest name = runLLVMWriter code main >>
    runLLVMAssembler main
    where main = dest </> name

buildRuntimeBC :: FilePath -> IO FilePath
buildRuntimeBC dest = runLLVMWriter runtimeLLVM runtime >>
    runLLVMAssembler runtime
    where runtime = dest </> "runtime"

buildExecutable :: [FilePath] -> FilePath -> IO FilePath
buildExecutable sources dest = runLLVMLinker sources linked >>
    runLLVMCompiler linked (dest </> "a")
    where linked = dest </> "linked"

runLLVMWriter :: String -> FilePath -> IO FilePath
runLLVMWriter ast file = do
    result <- execLLVMWriter ast $ llFile file
    case result of
        Left  e -> print e >> exitFailure
        Right _ -> return file

runLLVMAssembler :: FilePath -> IO FilePath
runLLVMAssembler file = runLLVMProg (llFile file)
    "ASSEMBLER" "llvm-as" [llFile file]

runLLVMLinker :: [FilePath]-> FilePath -> IO FilePath
runLLVMLinker files out = runLLVMProg (bcFile out)
    "LINKER" "llvm-link" $ fmap bcFile files ++ ["-o=" ++ bcFile out]

runLLVMCompiler :: FilePath -> FilePath -> IO FilePath
runLLVMCompiler file out = runLLVMProg (outFile out)
    "COMPILER" "llvm-gcc" [bcFile file, "-o", outFile out]

runLLVMProg :: FilePath -> String -> String -> [String] -> IO FilePath
runLLVMProg result phase cmd args = do
    (c,_,e) <- execLLVMProg cmd args
    if c /= ExitSuccess
        then llvmErr phase e >> exitFailure
        else return result

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
llFile = flip addExtension ".ll"
bcFile = flip addExtension ".bc"
outFile = flip addExtension ".out"