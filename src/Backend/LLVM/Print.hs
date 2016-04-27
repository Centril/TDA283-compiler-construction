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
Module      : Backend.LLVM.Print
Description : Print module for the LLVM AST of the Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Print module for the LLVM AST of the Javalette compiler.
-}
{-# LANGUAGE LambdaCase #-}

module Backend.LLVM.Print where

import Data.List

import Backend.LLVM.LLVMAst

u = undefined

type LLVMCode = String

--joinComma :: [[a]] -> [a]
joinComma = intercalate ", "

-- unLineList ::
unLineFun f a = unlines $ fmap f a

printLLVMAst :: LLVMAst -> LLVMCode
printLLVMAst (LLVMAst g c d) = unlines [unLineFun printConstGlobal g,
                                        unLineFun printFunDecl c,
                                        unLineFun printFunDef d]

printConstGlobal :: LConstGlobal -> LLVMCode
printConstGlobal (LConstGlobal i t v) =
    unwords [printIdentFun i, "=", "global", printType t, printValue v] ++ "\n"

printFunDecl :: LFunDecl ->  LLVMCode
printFunDecl (LFunDecl t i ts) =
    concat ["declare ", printType t, " ", printIdentFun i,
            "(", printTypes ts, ")\n"]

printFunDef :: LFunDef ->  LLVMCode
printFunDef (LFunDef t i as l) =
    concat ["\ndefine ", printType t, " ", printIdentFun i,
            "(", printArgs as, ") {\n", printLabels l, "\n}\n"]

printType :: LType -> LLVMCode
printType = \case
    LVoid      -> "void"
    LInt i     -> "i" ++ show i
    LFloat f   -> "f" ++ show f
    LPtr t     -> printType t ++ "*"
    LArray d t -> concat ["[",show d, " x ", printType t, "]*"]

printTypes :: LTypes -> LLVMCode
printTypes t = joinComma $ printType <$> t

printArg :: LArg -> LLVMCode
printArg (LArg t i) = printType t ++ " " ++ printIdentVar i

printArgs :: LArgs -> LLVMCode
printArgs a = joinComma $ printArg <$> a

printLabel :: LLabel -> LLVMCode
printLabel (LLabel i is) = printIdentPure i ++ ":\n" ++ printInsts is

printLabel' :: LIdent -> LLVMCode
printLabel' l = "label " ++ printIdentVar l

printLabels :: LLabels -> LLVMCode
printLabels = unLineFun printLabel

printOp :: LOp -> LLVMCode
printOp = \case
    LEq  -> "eq"
    LNe  -> "ne"
    LUlt -> "ult"
    LSgt -> "sgt"
    LUle -> "ule"
    LSge -> "sge"

printValRef :: LValRef -> LLVMCode
printValRef = \case
    LVInt i   -> show i
    LVFloat f -> show f
    LRef r    -> printIdentVar r

printTValRef :: LTValRef -> LLVMCode
printTValRef (LTValRef t r) = printType t ++ " " ++ printValRef r

printFunRef :: LFunRef -> LLVMCode
printFunRef (LFunRef i r) = printIdentFun i ++ " (" ++ printTValRef r ++ ")"

printInst :: LInst -> LLVMCode
printInst = \case
    LAssign i e  -> printIdentVar i ++ " = " ++ printExpr e
    LIExpr e     -> printExpr e
    LVCall fr    -> "call void " ++ printFunRef fr
    LABr l       -> "br " ++ printLabel' l
    LCBr r l1 l2 -> "br " ++ joinComma [printTValRef r, printLabel' l1,
                                        printLabel' l2]
    LRet r       -> "ret " ++ printTValRef r
    LStore r1 r2 -> "store " ++ joinComma [printTValRef r1, printTValRef r2]
    LUnreachable -> "unreachable"

printInstIndent :: LInst -> LLVMCode
printInstIndent i = "\n\t" ++  printInst i

printInsts :: LInsts -> LLVMCode
printInsts = unLineFun printInstIndent

printExpr :: LExpr -> LLVMCode
printExpr = \case
    LLoad r           -> unwords ["load", printTValRef r]
    LAlloca t         -> unwords ["alloca", printType t]
    LCall f           -> unwords ["load", printFunRef f]
    LBitcast t1 r t2  -> printToOp "bitcast" t1 r t2
    LAdd r1 r2        -> printMathOp "add" r1 r2
    LFAdd r1 r2       -> printMathOp "fadd" r1 r2
    LSub r1 r2        -> printMathOp "sub" r1 r2
    LFSub r1 r2       -> printMathOp "fsub" r1 r2
    LMul r1 r2        -> printMathOp "mul" r1 r2
    LFMul r1 r2       -> printMathOp "fmul" r1 r2
    LDiv r1 r2        -> printMathOp "div" r1 r2
    LFDiv r1 r2       -> printMathOp "fdiv" r1 r2
    LICmp o r1 r2     -> printCmp "icmp" o r1 r2
    LFCmp o r1 r2     -> printCmp "fcmp" o r1 r2
    LGElemPtr t i x y -> printGetPtr "getelementptr" t i x y
    LPtrToInt t1 r t2 -> printToOp "ptrtoint" t1 r t2

printMathOp :: LLVMCode -> LTValRef -> LValRef -> LLVMCode
printMathOp c r1 r2 = unwords [c, printTValRef r1, printValRef r2]

printCmp :: LLVMCode -> LOp -> LTValRef -> LValRef -> LLVMCode
printCmp c o r1 r2 = unwords [c, printOp o, printTValRef r1, printValRef r2]

printToOp :: LLVMCode -> LType -> LValRef -> LType -> LLVMCode
printToOp c t1 r t2 = unwords [c, printType t1, printValRef r, "to",
                                 printType t2]

printGetPtr :: LLVMCode -> LType -> LIdent -> LTIndex -> [LTIndex] -> LLVMCode
printGetPtr c t i x y = unwords [c, printType t, printIdentFun i,
                                   printTIndex x, printTIndexes y]

printIdentPure :: LIdent -> LLVMCode
printIdentPure = show

printIdentVar :: LIdent -> LLVMCode
printIdentVar i = "%" ++ show i

printIdentFun :: LIdent -> LLVMCode
printIdentFun i = "@" ++ show i

printIndex :: LIndex -> LLVMCode
printIndex = show

printTIndex :: (LType, LIndex) -> LLVMCode
printTIndex (t, i) = printType t ++ " " ++ printIndex i

printTIndexes :: [LTIndex] -> LLVMCode
printTIndexes t = joinComma $ printTIndex <$> t

printValue :: LValue -> LLVMCode
printValue str = "c\"" ++ show str ++ "\00"