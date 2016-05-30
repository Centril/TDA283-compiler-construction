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
Module      : Backend.PreGen
Description : Pre code gen targets for Javalette compiler
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Pre code gen targets for Javalette compiler
-}
module Backend.PreGen (
    -- * Phases    
    compileFrontend, compileAR, compilePO,

    -- * Targets
    targetTypeCheck, targetPreOpt, targetAlphaRename
) where

import Control.Monad
import Control.Monad.Reader

import Utils.Monad

import Common.Computation
import Common.FileOps

import Frontend.TypeCheck
import Frontend.ParseLex

import Backend.AlphaRename
import Backend.PreOptimize

--------------------------------------------------------------------------------
-- Targets:
--------------------------------------------------------------------------------

targetTypeCheck, targetAlphaRename, targetPreOpt :: JlcTarget
targetTypeCheck   = targetTcX compileFrontend
targetAlphaRename = targetTcX compileAR
targetPreOpt      = targetTcX compilePO

--------------------------------------------------------------------------------
-- Pre Code Gen:
--------------------------------------------------------------------------------

compileFrontend, compileAR, compilePO :: String -> TCComp ProgramA
compileFrontend = parseProgram                <<=> phaseEnd Parser >=>
                  typeCheck                   <<=> phaseEnd TypeChecker
compileAR = (compileFrontend >=> alphaRename) <<=> phaseEnd AlphaRenamer
compilePO = (compileAR >$> preOptimize)       <<=> phaseEnd PreOptimizer

--------------------------------------------------------------------------------
-- Utilities:
--------------------------------------------------------------------------------

targetTcX :: (String -> Comp TCEnv a) -> JlcTarget
targetTcX = targetX initialTCEnv

targetX :: s -> (String -> Comp s a) -> JlcTarget
targetX ienv cf = flip (evalIOComp $ compileXIO cf) ienv

compileXIO :: (String -> Comp s a) -> IOComp s ()
compileXIO cf = jlFiles . classifyInputs <$> ask
                >>= mapM_ (readF >=> rebase . cf)