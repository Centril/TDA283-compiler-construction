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
Module      : Backend.LLVM.LLVMGen
Description : LLVM code generator in the LLVM backend of the Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

LLVM code generator in the LLVM backend of the Javalette compiler.
-}
{-# LANGUAGE LambdaCase #-}

module Backend.LLVM.LLVMGen where

import Data.Map ((!))

import Control.Monad

import Control.Lens hiding (Empty, op)

import Utils.Shallow

import Common.AST

import Frontend.Annotations

import Backend.LLVM.Environment
import Backend.LLVM.LLVMAst
import Backend.LLVM.Print

u = undefined

compileLLVM :: ProgramA -> LComp LLVMAst
compileLLVM prog = do
    let tds = _pTopDefs prog
    LLVMAst [] [] <$> mapM compileFun tds

compileFun :: TopDefA -> LComp LFunDef
compileFun (FnDef _ rtyp name args block) = do
    let name' = compileFName name
    let rtyp' = compileFRTyp rtyp
    let args' = compileFArg <$> args
    insts <- compileFBlock block
    return $ LFunDef rtyp' name' args' insts

compileFName :: Ident -> LIdent
compileFName = _ident

compileFRTyp :: TypeA -> LType
compileFRTyp = compileType

compileFArg :: ArgA -> LArg
compileFArg arg = LArg (compileType $ _aTyp arg) (_ident $ _aIdent arg)

compileFBlock :: BlockA -> LComp LInsts
compileFBlock block = compileFBlock' block >> getInsts <* clearInsts

compileLabel :: LLabelRef -> LComp ()
compileLabel l = pushInst $ LLabel l

compileFBlock' :: BlockA -> LComp ()
compileFBlock' = (compileLabel "entry" >>) . compileBlock

compileBlock :: BlockA -> LComp ()
compileBlock = mapM_ compileStmt . _bStmts

compileStmt :: StmtA -> LComp ()
compileStmt = \case
    Empty    _         -> return ()
    BStmt    _ b       -> compileBlock b
    Decl     _ t is    -> forM_ is $ compileDecl t
    Ass      _ i e     -> compileAss i e
    Incr     _ i       -> u
    Decr     _ i       -> u
    Ret      _ e       -> compileRet e
    VRet     _         -> pushInst LVRet
    Cond     _ c si    -> compileCond     c si
    CondElse _ c si se -> compileCondElse c si se
    While    _ c sw    -> compileWhile    c sw
    SExp     _ e       -> void $ compileExpr e

compileDecl :: TypeA -> ItemA -> LComp ()
compileDecl typ item = do
    let name = _iIdent item
    pushInst $ LAssign (_ident name) $ LAlloca (compileType typ)
    case item of
        NoInit _ i   -> return ()
        Init   _ i e -> compileAss name e

compileAss :: Ident -> ExprA -> LComp ()
compileAss name e = do
    e' <- compileExpr e
    pushInst $ LStore e' $
               LTValRef (LPtr $ _lTType e') (LRef $ _ident name)

compileRet :: ExprA -> LComp ()
compileRet e = LRet <$> compileExpr e >>= pushInst

compileCond :: ExprA -> StmtA -> LComp ()
compileCond c si = do
    _then <- newLabel "then"
    _cont <- newLabel "cont"
    compileCondExpr c  _then _cont
    compileCondStmt si _then _cont
    compileLabel             _cont

compileCondElse :: ExprA -> StmtA -> StmtA -> LComp ()
compileCondElse c si se = do
    _then <- newLabel "then"
    _else <- newLabel "else"
    _cont <- newLabel "cont"
    compileCondExpr c  _then _else
    compileCondStmt si _then _cont
    compileCondStmt se _else _cont
    compileLabel             _cont

compileWhile :: ExprA -> StmtA -> LComp ()
compileWhile c sw = do
    _while <- newLabel "while"
    _then  <- newLabel "then"
    _cont  <- newLabel "cont"
    compileLabel             _while
    compileCondExpr c  _then _cont
    compileCondStmt sw _then _while
    compileLabel             _cont

compileCondExpr :: ExprA -> LLabelRef -> LLabelRef -> LComp ()
compileCondExpr c _then _else = do
    c' <- compileExpr c
    pushInst $ LCBr c' _then _else

compileCondStmt :: StmtA -> LLabelRef -> LLabelRef -> LComp ()
compileCondStmt stmt _then _cont = do
    compileLabel _then
    compileStmt stmt
    pushInst $ LABr _cont

compileExpr :: ExprA -> LComp LTValRef
compileExpr = \case
    EVar      a i     -> compileEVar a i
    ELitInt   _ v     -> compileLInt   sizeofInt   v
    ELitDoub  _ v     -> compileLFloat sizeofFloat v
    EString   _ v     -> compileCString v
    ELitTrue  _       -> compileLInt   sizeofBool  1
    ELitFalse _       -> compileLInt   sizeofBool  0
    EApp      a i es  -> compileApp a i es
    Neg       _ e     -> u
    Not       _ e     -> compileNot e
    EMul      _ l o r -> u
    EAdd      _ l o r -> u
    ERel      _ l o r -> u
    EAnd      _ l   r -> u
    EOr       _ l   r -> u

compileEVar :: ASTAnots -> Ident -> LComp LTValRef
compileEVar anots name = do
    let typ = compileAnotType anots
    assignTemp typ $ LLoad $ LTValRef (LPtr typ) (LRef $ _ident name)

compileNot :: ExprA -> LComp LTValRef
compileNot e = compileExpr e >>= assignTemp boolType . flip LXor (LVInt 1)

compileApp :: ASTAnots -> Ident -> [ExprA] -> LComp LTValRef
compileApp anots name es = do
    fr <- LFunRef (_ident name) <$> mapM compileExpr es
    case compileAnotType anots of
        LVoid -> pushInst (LVCall fr) >> return (LTValRef LVoid LNull)
        rtyp  -> assignTemp rtyp $ LCall rtyp fr

assignTemp :: LType -> LExpr -> LComp LTValRef
assignTemp rtyp expr = do
    temp <- newTemp
    pushInst $ LAssign temp expr
    return   $ LTValRef rtyp $ LRef temp

compileAnotType :: ASTAnots -> LType
compileAnotType anots = let AType typ = anots ! AKType in compileType typ

compileCString :: String -> LComp LTValRef
compileCString v = do
    temp <- newTemp
    ref  <- newConstRef "cstring"
    let typ   = LArray (1 + length v) charType
    pushConst $ LConstGlobal ref typ v
    pushInst  $ LAssign temp (strPointer (LPtr typ) ref)
    return    $ LTValRef strType $ LRef ref

compileType :: TypeA -> LType
compileType = \case
    Int      _ -> LInt   sizeofInt
    Doub     _ -> LFloat sizeofFloat
    Bool     _ -> LInt   sizeofBool
    Void     _ -> LVoid
    ConstStr _ -> strType
    Fun      _ rtyp argst -> u

boolType, charType, strType :: LType
boolType = LInt sizeofBool
charType = LInt sizeofChar
strType  = LPtr charType

zeroIndex :: LTIndex
zeroIndex = (LInt 32, 0)

strPointer :: LType -> LIdent -> LExpr
strPointer t i = LGElemPtr t i zeroIndex [zeroIndex]

compileLInt :: Int -> Integer -> LComp LTValRef
compileLInt s v = return $ LTValRef (LInt s) (LVInt v)

compileLFloat :: Int -> Double -> LComp LTValRef
compileLFloat s v = return $ LTValRef (LFloat s) (LVFloat v)

sizeofBool, sizeofChar, sizeofInt, sizeofFloat :: Int
sizeofBool  = 1
sizeofChar  = 8
sizeofInt   = 32
sizeofFloat = 64