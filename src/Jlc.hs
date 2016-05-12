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

import Control.Monad

import Control.Lens

import System.FilePath
import System.Exit ( exitFailure )

import Utils.Monad
import Utils.Pointless
import Utils.Terminal

import Common.Computation

import Frontend.ParseLex
import Frontend.TypeCheck

import Backend.AlphaRename
import Backend.PreOptimize

import Backend.LLVM.LLVMGen
import Backend.LLVM.LLVMApi

import CliOptions

--------------------------------------------------------------------------------
-- main:
--------------------------------------------------------------------------------

main :: IO ()
main = do
    opts <- compOptions
    if _typecheckOnly opts
    then typecheckTarget opts >>= compileCheck
    else compileUnit opts

--------------------------------------------------------------------------------
-- Compilation of units:
--------------------------------------------------------------------------------

compileUnit :: JlcOptions -> IO ()
compileUnit = runCompile >=> compileCheck

runCompile :: JlcOptions -> IOLResult ()
runCompile opts = runIOComp (compileBuildIO initialTCEnv) opts initialLEnv

preCodeGen :: String -> TCComp ProgramA
preCodeGen code = do
    ast1 <- compileTC code
    let ast2 = alphaRename ast1
    infoP AlphaRenamer "AST after alpha rename" ast2
    let ast3 = preOptimize ast2
    infoP PreOptimizer "AST after pre optimizing" ast3
    return ast3

compile :: String -> TCEnv -> LComp LLVMCode
compile = changeST . preCodeGen >?=> compileLLVM

compileIO :: String -> TCEnv -> IOLComp LLVMCode
compileIO = rebase .| compile

compileBuildIO :: TCEnv -> IOLComp ()
compileBuildIO env = do
    code <- head <$> inputSources
    compileIO code env >>= build

-- TODO: Handle command line options instead of head...
build :: LLVMCode -> IOLComp ()
build code = do
    file <- head <$> view inputFiles
    io $ void $ buildMainBC code (takeDirectory file) (takeBaseName file)

--------------------------------------------------------------------------------
-- Typecheck target:
--------------------------------------------------------------------------------

typecheckTarget :: JlcOptions -> IOCompResult TCEnv ()
typecheckTarget = flip (runIOComp compileTCIO) initialTCEnv

compileTCIO :: IOComp TCEnv ()
compileTCIO = inputSources >>= mapM_ (rebase . compileTC)

compileTC :: String -> TCComp ProgramA
compileTC code = do
    ast1 <- parseProgram code <<= infoP Parser      "AST after parse"
    typeCheck ast1            <<= phaseEnd TypeChecker

--------------------------------------------------------------------------------
-- Helpers:
--------------------------------------------------------------------------------

inputSources :: IOComp s [String]
inputSources = view inputFiles >>= mapM (io . readFile)

compileCheck :: (Show r, Show e) => CompResult e r -> IO ()
compileCheck = uncurry $ either compileUnitFail compileUnitSucc

compileUnitFail :: ErrMsg -> InfoLog -> IO ()
compileUnitFail eMsg logs = do
    errLn $ phaseErr $ errPhase eMsg
    printError eMsg >> printLogs logs
    exitFailure

compileUnitSucc :: (a, b) -> InfoLog -> IO ()
compileUnitSucc (_, _) logs = printLogs logs >> errLn "OK"

phaseErr :: Phase -> String
phaseErr Parser        = "SYNTAX ERROR"
phaseErr TypeChecker   = "TYPE ERROR"
phaseErr ReturnChecker = "TYPE ERROR"
phaseErr AlphaRenamer  = "ALPHA-RENAME ERROR"
phaseErr PreOptimizer  = "PRE-OPTIMIZE ERROR"
phaseErr Compiler      = "COMPILE ERROR"
phaseErr Linker        = "LINK ERROR"