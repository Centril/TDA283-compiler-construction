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

import Backend.LLVM.LLVMAst
import Backend.LLVM.Print

compileFilePath :: String -> FilePath
compileFilePath = undefined

createExecutable :: LLVMAst -> IO (Maybe FilePath)
createExecutable ast = undefined

runLLVMAssembler :: IO (Maybe FilePath) -> IO (Maybe FilePath)
runLLVMAssembler = undefined

runLLVMLinker :: IO (Maybe FilePath) -> IO (Maybe FilePath)
runLLVMLinker = undefined

runLLVMCompiler :: IO (Maybe FilePath) -> IO (Maybe FilePath)
runLLVMCompiler = undefined