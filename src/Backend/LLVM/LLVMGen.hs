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

import Control.Monad

import Control.Lens hiding (Empty, op)

import Common.AST

import Frontend.Annotations

import Backend.LLVM.Environment
import Backend.LLVM.LLVMAst

u = undefined

compileLLVM :: ProgramA -> LComp LLVMAst
compileLLVM prog = do
    let tds = _pTopDefs prog
    LLVMAst [] [] <$> mapM compileFun tds

compileFun :: TopDefA -> LComp LFunDef
compileFun (FnDef _ rtyp name args block) = do
    name' <- compileFName name
    rtyp' <- compileFRTyp rtyp
    args' <- mapM compileFArg args
    insts <- compileFBlock block

    return $ LFunDef rtyp' name' args' insts

compileFName :: Ident -> LComp LIdent
compileFName = pure . _ident

compileFRTyp :: TypeA -> LComp LType
compileFRTyp = u

compileFArg :: ArgA -> LComp LArg
compileFArg = u

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
    VRet     _         -> u
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

compileVRet :: LComp ()
compileVRet = u

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
compileExpr = u

compileType :: TypeA -> LType
compileType = \case
    Int      _ -> LInt   32
    Doub     _ -> LFloat 32
    Bool     _ -> LInt   1
    Void     _ -> LVoid
    ConstStr _ -> u
    Fun      _ rtyp argst -> u