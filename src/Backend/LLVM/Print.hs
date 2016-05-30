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

module Backend.LLVM.Print (
    -- * Operations
    printLLVMAst
) where

import Data.Char
import Data.List

import Utils.Pointless

import Backend.LLVM.AST

printLLVMAst :: LLVMAst -> LLVMCode
printLLVMAst (LLVMAst g a c d) =
    trim $ unlines [unlineFun printConstGlobal g
                   ,unlineFun printAlias a
                   ,unlineFun printFunDecl c
                   ,unlineFun printFunDef d]

joinComma :: [String] -> String
joinComma = intercalate ", "

parens, block, btype, bracket, ws :: String -> String
parens  x = "(" ++ x ++ ")"
block   x = "{\n" ++ x ++ "}\n"
btype   x = "{" ++ x ++ "}"
bracket x = "[" ++ x ++ "]"
ws      x = " " ++ x ++ " "

trim :: String -> String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

-- from: http://stackoverflow.com/questions/6270324
dropSpaceTail :: String -> String -> String
dropSpaceTail _    "" = ""
dropSpaceTail maybeStuff (x:xs)
    | isSpace x       = dropSpaceTail (x:maybeStuff) xs
    | null maybeStuff = x : dropSpaceTail "" xs
    | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

unlineFun :: (a -> String) -> [a] -> String
unlineFun = unlines .| (<$>)

printConstGlobal :: LConstGlobal -> LLVMCode
printConstGlobal (LConstGlobal i t v) =
    unwords [printIdentFun i, "=", "global", printType t, printValue v]

printVTableGlobal :: LIdent -> [(LType, LIdent)] -> LLVMCode
printVTableGlobal ident methods =
    unwords [printIdentFun ident, "=", "global", printType (LAlias ident),
             block $ unlines $ printVTableDefs <$> methods]

printVTableDefs :: (LType, LIdent) -> LLVMCode
printVTableDefs (typ, ident) = unwords [" ", printType typ, printIdentFun ident]

printAlias :: LAlias -> LLVMCode
printAlias (r, t) = unwords [printAliasRef r, "=", "type", printType t]

printAliasRef :: LAliasRef -> LLVMCode
printAliasRef r = "%" ++ r

printFunDecl :: LFunDecl ->  LLVMCode
printFunDecl (LFunDecl t i ts) =
    unwords ["declare", printType t, printIdentFun i, parens $ printTypes ts]

printFunDef :: LFunDef ->  LLVMCode
printFunDef (LFunDef t i as is) =
    unwords ["define", printType t, printIdentFun i,
             parens $ printArgs as, block $ printInsts is]

-- TODO: Figure out double and float in LLVM
printType :: LType -> LLVMCode
printType = \case
    LVoid       -> "void"
    LAlias r    -> "%" ++ r
    LInt i      -> "i" ++ show i
    LFloat _    -> "double"
    LPtr t      -> printType t ++ "*"
    LFunPtr r a -> unwords [printType r, parens (printTypes a) ++ "*"]
    LArray d t  -> bracket $ unwords [show d, "x", printType t]
    LStruct ts  -> btype $ printTypes ts
    LInd        -> "..."

printTypes :: LTypes -> LLVMCode
printTypes t = joinComma $ printType <$> t

printArg :: LArg -> LLVMCode
printArg (LArg t i) = printType t ++ " " ++ printIdentVar i

printArgs :: LArgs -> LLVMCode
printArgs a = joinComma $ printArg <$> a

printLabel :: LIdent -> LLVMCode
printLabel lr = "label " ++ printLabRef lr

printIcmpOp :: LICmpOp -> LLVMCode
printIcmpOp = \case
    LEq  -> "eq"
    LNe  -> "ne"
    LUgt -> "ugt"
    LUge -> "uge"
    LUlt -> "ult"
    LUle -> "ule"
    LSlt -> "slt"
    LSgt -> "sgt"
    LSle -> "sle"
    LSge -> "sge"

printFcmpOp :: LFCmpOp -> LLVMCode
printFcmpOp = \case
    LFOeq -> "oeq"
    LFOgt -> "ogt"
    LFOge -> "oge"
    LFOlt -> "olt"
    LFOle -> "ole"
    LFOne -> "one"
    LFOrd -> "ord"
    LFUeq -> "ueq"
    LFUgt -> "ugt"
    LFUge -> "uge"
    LFUlt -> "ult"
    LFUle -> "ule"
    LFUne -> "une"
    LFUno -> "uno"

printValRef :: LValRef -> LLVMCode
printValRef = \case
    LVInt   i -> show i
    LVFloat f -> show f
    LRef    r -> printIdentVar r
    LConst  c -> printIdentFun c
    LNull     -> "null"

printTValRef :: LTValRef -> LLVMCode
printTValRef (LTValRef t r) = printType t ++ " " ++ printValRef r

printFunRef :: LFunRef -> LLVMCode
printFunRef (LFunRef dyn i args) =
    pif i ++ parens (joinComma $ printTValRef <$> args)
    where pif = if dyn then printLabRef else printIdentFun

printInst :: LInst -> LLVMCode
printInst = \case
    LAssign i e  -> printIdentVar i ++ " = " ++ printExpr e
    LIExpr e     -> printExpr e
    LVCall fr    -> "call void " ++ printFunRef fr
    LABr i       -> "br " ++ printLabel i
    LCBr r i1 i2 -> "br " ++ joinComma [printTValRef r, printLabel i1,
                                        printLabel i2]
    LVRet        -> "ret void"
    LRet r       -> "ret " ++ printTValRef r
    LStore r1 r2 -> "store " ++ joinComma [printTValRef r1, printTValRef r2]
    LUnreachable -> "unreachable"
    LLabel _     -> error ""

printInstIndent :: LInst -> LLVMCode
printInstIndent = \case
    LLabel l -> printIdent l ++ ":"
    i        -> "  " ++ printInst i

printInsts :: LInsts -> LLVMCode
printInsts = unlineFun printInstIndent

printExpr :: LExpr -> LLVMCode
printExpr = \case
    LLoad r           -> unwords ["load", printTValRef r]
    LAlloca t         -> unwords ["alloca", printType t]
    LCall t fr        -> unwords ["call", printType t, printFunRef fr]
    LBitcast r1 to    -> printToOp "bitcast" r1 to
    LAdd r1 r2        -> printMathOp "add" r1 r2
    LFAdd r1 r2       -> printMathOp "fadd" r1 r2
    LSub r1 r2        -> printMathOp "sub" r1 r2
    LFSub r1 r2       -> printMathOp "fsub" r1 r2
    LMul r1 r2        -> printMathOp "mul" r1 r2
    LFMul r1 r2       -> printMathOp "fmul" r1 r2
    LDiv r1 r2        -> printMathOp "sdiv" r1 r2
    LFDiv r1 r2       -> printMathOp "fdiv" r1 r2
    LSRem r1 r2       -> printMathOp "srem" r1 r2
    LFRem r1 r2       -> printMathOp "frem" r1 r2
    LXor  r1 r2       -> printMathOp "xor" r1 r2
    LPhi  t rs        -> printPhi t rs
    LICmp o r1 r2     -> printCmp "icmp" (printIcmpOp o) r1 r2
    LFCmp o r1 r2     -> printCmp "fcmp" (printFcmpOp o) r1 r2
    LGElemPtr r1 x ys -> printGetPtr "getelementptr" r1 x ys
    LPtrToInt r1 to   -> printToOp "ptrtoint" r1 to

printPhi :: LType -> LPhiRefs -> String
printPhi t rs = unwords ["phi", printType t, joinComma $ printPR <$> rs]
    where printPR (LPhiRef vr lr) =
            bracket $ ws $ joinComma [printValRef vr, printLabRef lr]

printMathOp :: LLVMCode -> LTValRef -> LValRef -> LLVMCode
printMathOp c r1 r2 = unwords [c, printTValRef r1 ++ ",", printValRef r2]

printCmp :: LLVMCode -> LLVMCode -> LTValRef -> LValRef -> LLVMCode
printCmp c o r1 r2 = unwords [c, o, printTValRef r1 ++ ",", printValRef r2]

printToOp :: LLVMCode -> LTValRef -> LType -> LLVMCode
printToOp c r1 to = unwords [c, printTValRef r1, "to", printType to]

printGetPtr :: LLVMCode -> LTValRef -> LTValRef -> LTValRefs -> LLVMCode
printGetPtr c r1 i is = unwords [c, joinComma $ printTValRef <$> (r1:i:is)]

printIdent :: LIdent -> LLVMCode
printIdent = id

printIdentVar :: LIdent -> LLVMCode
printIdentVar i = "%" ++ i

printLabRef :: LLabelRef -> LLVMCode
printLabRef lr = "%" ++ lr

printIdentFun :: LIdent -> LLVMCode
printIdentFun i = "@" ++ i

printValue :: LValue -> LLVMCode
printValue str = "c\"" ++  str ++ "\\00\""