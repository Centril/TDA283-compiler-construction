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
module Main where

import System.Exit

import Utils.Terminal

import Common.Computation

import Frontend.TypeCheck

import Backend.LLVM.LLVM

import CliOptions

--------------------------------------------------------------------------------
-- main:
--------------------------------------------------------------------------------

main :: IO ()
main = do
    opts <- compOptions
    determineTarget opts opts >>= evalCheck

determineTarget :: JlcOptions -> JlcTarget
determineTarget      opts
    | _preOptOnly    opts = targetPreOpt
    | _aRenameOnly   opts = targetAlphaRename
    | _typecheckOnly opts = targetTypeCheck
    | otherwise           = targetLLVM

--------------------------------------------------------------------------------
-- Helpers:
--------------------------------------------------------------------------------

evalCheck :: CompEval a -> IO ()
evalCheck = uncurry $ either evalFailure evalSuccess

evalFailure :: ErrMsg -> InfoLog -> IO ()
evalFailure eMsg logs = do
    errLn $ phaseErr $ errPhase eMsg
    printLogs logs >> printError eMsg
    exitFailure

evalSuccess :: a -> InfoLog -> IO ()
evalSuccess _ logs = printLogs logs >> errLn "OK"

phaseErr :: Phase -> String
phaseErr Parser        = "SYNTAX ERROR"
phaseErr TypeChecker   = "TYPE ERROR"
phaseErr ReturnChecker = "TYPE ERROR"
phaseErr AlphaRenamer  = "ALPHA-RENAME ERROR"
phaseErr PreOptimizer  = "PRE-OPTIMIZE ERROR"
phaseErr Compiler      = "COMPILE ERROR"
phaseErr Linker        = "LINK ERROR"