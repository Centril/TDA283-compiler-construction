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
Module      : Backend.JRuntime
Description : The Javalette LLVM runtime AST of the Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

The Javalette LLVM runtime AST of the Javalette compiler.
-}

module Backend.LLVM.JRuntime where

import Backend.LLVM.LLVMAst

--------------------------------------------------------------------------------
-- From contrib/runtime.ll
--------------------------------------------------------------------------------

-- TODO: Implement internal constant
-- TODO: Implement AST functions
-- TODO: Remove runtimeLLVM

runtimeLLVMAst :: LLVMAst
runtimeLLVMAst = LLVMAst
    [LConstGlobal "dnl" (LArray 4 (LInt 8)) "%d\0A",
     LConstGlobal "fnl" (LArray 4 (LInt 8)) "%.1f\0A",
     LConstGlobal "d"   (LArray 4 (LInt 8)) "%d",
     LConstGlobal "lf"  (LArray 4 (LInt 8)) "%lf"]
    [LFunDecl (LInt 32) "printf" [LPtr (LInt 8), LInd],
     LFunDecl (LInt 32) "scanf"  [LPtr (LInt 8), LInd],
     LFunDecl (LInt 32) "puts"   [LPtr (LInt 8)]]
     []

runtimeLLVM :: String
runtimeLLVM = unlines [
    "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"",
    "@fnl = internal constant [6 x i8] c\"%.1f\\0A\\00\"",
    "@d   = internal constant [3 x i8] c\"%d\\00\"",
    "@lf  = internal constant [4 x i8] c\"%lf\\00\"",
    "",
    "declare i32 @printf(i8*, ...)",
    "declare i32 @scanf(i8*, ...)",
    "declare i32 @puts(i8*)",
    "",
    "define void @printInt(i32 %x) {",
    "entry: %t0 = getelementptr [4 x i8]* @dnl, i32 0, i32 0",
    "  call i32 (i8*, ...)* @printf(i8* %t0, i32 %x)",
    "  ret void",
    "}",
    "",
    "define void @printDouble(double %x) {",
    "entry: %t0 = getelementptr [6 x i8]* @fnl, i32 0, i32 0",
    "  call i32 (i8*, ...)* @printf(i8* %t0, double %x)",
    "  ret void",
    "}",
    "",
    "define void @printString(i8* %s) {",
    "entry:  call i32 @puts(i8* %s)",
    "  ret void",
    "}",
    "",
    "define i32 @readInt() {",
    "entry: %res = alloca i32",
    "  %t1 = getelementptr [3 x i8]* @d, i32 0, i32 0",
    "  call i32 (i8*, ...)* @scanf(i8* %t1, i32* %res)",
    "  %t2 = load i32* %res",
    "  ret i32 %t2",
    "}",
    "",
    "define double @readDouble() {",
    "entry: %res = alloca double",
    "  %t1 = getelementptr [4 x i8]* @lf, i32 0, i32 0",
    "  call i32 (i8*, ...)* @scanf(i8* %t1, double* %res)",
    "  %t2 = load double* %res",
    "  ret double %t2",
    "}"]