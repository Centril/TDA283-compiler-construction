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
Description : LLVM backend target for Javalette compiler
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

LLVM backend target for Javalette compiler
-}

{-# LANGUAGE LambdaCase #-}

module Backend.LLVM.LLVMApi (
    -- * Operations
    targetLLVM
) where

import Control.Arrow

import Control.Monad
import Control.Monad.Reader

import Control.Lens hiding ((<.>))

import System.FilePath

import Utils.Monad

import Common.Computation
import Common.FileOps

import Frontend.TypeCheck

import Backend.AlphaRename
import Backend.PreOptimize

import Backend.LLVM.LLVMGen

targetLLVM :: JlcTarget
targetLLVM opts = evalIOComp compileLLIO opts initialLEnv

compileLLIO :: IOLComp ()
compileLLIO = do
    (out, ifm) <- (determineOutput &&& classifyInputs) <$> ask
    lls        <- mapM compileLL (jlFiles ifm) <++> mapM readF (llFiles ifm)
    withSysTempDir "jlc" $ \tmp -> do
        bcs    <- (bcFiles ifm ++) <$> zipWithM (llvmAssemble tmp) [1..] lls
        let bcLinked = bcFile tmp "linked"
        llvmLink bcs bcLinked >> llvmOpt bcLinked
        view outFileType >>= \case
            OFTBitcode -> copyF bcLinked out
            OFTExec    -> let tmpObj = tmp </> "linked" <.> "o"
                          in llvmLLC "obj" bcLinked tmpObj >> llvmGCC tmpObj out
            OFTAsm     -> llvmLLC "asm" bcLinked out

compileLL :: FilePath -> IOLComp LLVMCode
compileLL = fkeep (readF >=> rebase . compile) >=> \(orig, ll) -> do
    inter <- view llIntermed
    when inter $ writeF (orig -<.> "ll") ll
    return ll

compile :: String -> LComp LLVMCode
compile = flip (changeST . preCodeGen) initialTCEnv >=> compileLLVM

llvmAssemble :: FilePath -> Int -> LLVMCode -> IOLComp FilePath
llvmAssemble tmp count ll = let fp = bcFile tmp $ show count
                            in llvmAs fp ll >> return fp

bcFile :: FilePath -> FilePath -> FilePath
bcFile dir name = dir </> name <.> "bc"

--------------------------------------------------------------------------------
-- Pre Code Gen:
--------------------------------------------------------------------------------

preCodeGen :: String -> TCComp ProgramA
preCodeGen code = do
    ast1 <- compileFrontend code
    let ast2 = alphaRename ast1
    infoP AlphaRenamer "AST after alpha rename" ast2
    let ast3 = preOptimize ast2
    infoP PreOptimizer "AST after pre optimizing" ast3
    return ast3

--------------------------------------------------------------------------------
-- LLVM: External programs:
--------------------------------------------------------------------------------

llvmAs :: FilePath -> LLVMCode -> IOLComp ()
llvmAs fp = void . execProcess (err Compiler) "llvm-as" ["-o", fp]

llvmGCC :: FilePath -> FilePath -> IOLComp ()
llvmGCC = execLink "gcc" [] . pure

llvmLLC :: String -> FilePath -> FilePath -> IOLComp ()
llvmLLC ftype = execLink "llc" ["-filetype", ftype, "--march=x86-64"] . pure

llvmLink :: [FilePath] -> FilePath -> IOLComp ()
llvmLink = execLink "llvm-link" []

llvmOpt :: FilePath -> IOLComp ()
llvmOpt inp = do
    optFlag <- view optLevel
    unless (optFlag == Optimize0) $
        execLink "opt" [determineOptFlag optFlag] [inp] inp

determineOptFlag :: OptLevel -> String
determineOptFlag = \case
    Optimize0 -> "O0" -- not used, for totality.
    Optimize1 -> "O1"
    Optimize2 -> "O2"
    Optimize3 -> "Os"
    Optimize4 -> "Oz"
    Optimize5 -> "O3"
    Optimize6 -> "O3"

execLink :: FilePath -> [String] -> [FilePath] -> FilePath -> IOComp s ()
execLink = execInOut (err Linker)