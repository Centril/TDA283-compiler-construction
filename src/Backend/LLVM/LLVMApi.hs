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

--------------------------------------------------------------------------------
-- LLVM phases with error handling:
--------------------------------------------------------------------------------

buildExecutable :: LLVMAst -> FilePath -> IO ()
buildExecutable ast file = do
    l1 <- runLLVMWriter ast file
    l2 <- runLLVMAssembler file
    -- l3 <- runLLVMLinker file -- TODO: Implement
    l4 <- runLLVMCompiler file
    return ()

runLLVMWriter :: LLVMAst -> FilePath -> IO ()
runLLVMWriter ast file = do
    result <- execLLVMWriter ast $ addExtension file ".ll"
    case result of
        Left  e -> print e
        Right _ -> return ()

runLLVMAssembler :: FilePath -> IO ()
runLLVMAssembler = runLLVMProg "ASSEMBLER" "llvm-as" ".ll"

runLLVMLinker :: FilePath -> IO ()
runLLVMLinker = runLLVMProg "LINKER" "llvm-link" ".bc"

runLLVMCompiler :: FilePath -> IO ()
runLLVMCompiler = runLLVMProg "COMPILER" "llvm-gcc" ".bc"

runLLVMProg :: String -> String -> String -> FilePath -> IO ()
runLLVMProg phase cmd ext file = do
    (c,o,e) <- execLLVMProg cmd $ addExtension file ext
    when (c /= ExitSuccess) $ llvmErr phase e

--------------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------------

execLLVMWriter :: LLVMAst -> FilePath -> IO (Either SomeException ())
execLLVMWriter ast file = try $ writeFile file $ printLLVMAst ast

execLLVMProg :: String -> FilePath -> IO (ExitCode, String, String)
execLLVMProg prog file = readProcessWithExitCode prog [file] []

llvmErr :: String -> FilePath -> IO()
llvmErr s ex = die $ unwords ["LLVM", s, "ERROR", ex]

addExtension :: FilePath -> String -> FilePath
addExtension file ext = file ++ ext