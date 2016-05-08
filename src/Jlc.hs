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

import System.Environment ( getArgs )
import System.Exit ( exitFailure )

import Control.Monad

import Utils.Monad
import Utils.Pointless
import Utils.Terminal

import Common.Computation

import Frontend.ParseLex
import Frontend.TypeCheck

import Backend.AlphaRename
import Backend.PreOptimize

import Backend.LLVM.LLVMGen

--------------------------------------------------------------------------------
-- main:
--------------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= handleArgs >>= compileUnit

--------------------------------------------------------------------------------
-- Compilation of units:
--------------------------------------------------------------------------------

compileUnit :: String -> IO ()
compileUnit = runCompile >=> uncurry (either compileUnitFail compileUnitSucc)

compileUnitFail :: ErrMsg -> InfoLog -> IO ()
compileUnitFail eMsg logs = do
    errLn $ phaseErr $ errPhase eMsg
    printError eMsg >> printLogs logs
    exitFailure

compileUnitSucc :: (LLVMCode, LEnv) -> InfoLog -> IO ()
compileUnitSucc (val, env) logs = do
    putStrLn "COMPILATION SUCCESS!"
    putStrLn "Accumulated logs:"        >> printLogs logs
    putStrLn "Final computed value:"    >> poutput val
    putStrLn "Final environment value:" >> poutput env
    errLn "OK"

runCompile :: String -> IOLResult LLVMCode
runCompile code = runIOComp (compileIO code initialTCEnv) initialLEnv

preCodeGen :: String -> TCComp ProgramA
preCodeGen code = do
    ast1 <- parseProgram code
    infoP Parser "AST after parse" ast1
    ast2 <- typeCheck ast1
    infoP TypeChecker "AST after type check" ast2
    let ast3 = alphaRename ast2
    infoP AlphaRenamer "AST after alpha rename" ast3
    let ast4 = preOptimize ast3
    infoP PreOptimizer "AST after pre optimizing" ast4
    return ast4

compile :: String -> TCEnv -> LComp LLVMCode
compile = changeST . preCodeGen >?=> compileLLVM

compileIO :: String -> TCEnv -> IOLComp LLVMCode
compileIO = rebase .| compile

--------------------------------------------------------------------------------
-- Helpers:
--------------------------------------------------------------------------------

phaseErr :: Phase -> String
phaseErr Parser        = "SYNTAX ERROR"
phaseErr TypeChecker   = "TYPE ERROR"
phaseErr ReturnChecker = "TYPE ERROR"
phaseErr AlphaRenamer  = "ALPHA-RENAME ERROR"
phaseErr PreOptimizer  = "PRE-OPTIMIZE ERROR"
phaseErr Compiler      = "COMPILE ERROR"
phaseErr Linker        = "LINK ERROR"