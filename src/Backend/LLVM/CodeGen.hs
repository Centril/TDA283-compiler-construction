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
Module      : Backend.LLVM.CodeGen
Description : LLVM code generator in the LLVM backend of the Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

LLVM code generator in the LLVM backend of the Javalette compiler.
-}
{-# LANGUAGE LambdaCase, TupleSections #-}

module Backend.LLVM.CodeGen (
    -- * Modules
    module Backend.LLVM.Environment,
    module Backend.LLVM.Print,

    -- * Operations
    compileLLVM
) where

import Safe

import Data.Maybe
import qualified Data.Map as M

import Control.Monad

import Control.Lens hiding (Empty, op, uncons, to, pre, ix)

import Utils.Monad
import qualified Utils.GraphFlip as GF

import Common.AST
import Common.Annotations
import Common.ASTOps

import Backend.LLVM.Environment
import Backend.LLVM.Types
import Backend.LLVM.CodeDSL
import Backend.LLVM.CodeComplex
import Backend.LLVM.CodeExpr
import Backend.LLVM.Print

import qualified Frontend.Environment as F

compileLLVM :: ProgramA -> LComp LLVMCode
compileLLVM = compileLLVMAst >$> printLLVMAst

compileLLVMAst :: ProgramA -> LComp LLVMAst
compileLLVMAst = compileFuns' >=>
    liftM4 LLVMAst getConsts allAliases (return predefDecls) . return

predefDecls :: LFunDecls
predefDecls =
    [ LFunDecl bytePType "calloc"      [intType, intType]
    , LFunDecl LVoid     "free"        [bytePType]
    , LFunDecl LVoid     "printInt"    [intType]
    , LFunDecl LVoid     "printDouble" [doubType]
    , LFunDecl LVoid     "printString" [strType]
    , LFunDecl intType   "readInt"     []
    , LFunDecl doubType  "readDouble"  []]

compileFuns' :: ProgramA -> LComp LFunDefs
compileFuns' = liftM2 (++) compileClasses . compileFuns

compileFuns :: ProgramA -> LComp LFunDefs
compileFuns = intoProg _TFnDef $ \(FnDef _ rtyp name args block) ->
    compileFun name rtyp args block

compileFun :: Ident -> TypeA -> [ArgA] -> BlockA -> LComp LFunDef
compileFun name rtyp args block = do
    let name'  = compileFName name
    rtyp'     <- compileFRTyp rtyp
    args'     <- mapM compileFArg args
    insts     <- compileFBlock args block
    let insts' = insts ++ unreachableMay block
    return $ LFunDef rtyp' name' args' insts'

compileClasses :: LComp LFunDefs
compileClasses = use classGraph >>= \(_, gr) -> do
    let cls = reverse $ GF.topsort' gr
    concat <$> mapM compileClass cls

compileClass :: F.ClassInfo -> LComp LFunDefs
compileClass cl = do
    currClass .= Just cl
    funs     <- mapM (compileMethod cl) $ M.elems $ F._ciMethods cl
    currClass .= Nothing
    return funs

compileMethod :: F.ClassInfo -> FnDefA -> LComp LFunDef
compileMethod cl meth@(FnDef _ rtyp _ args block) = do
    let name  = nameMethod (meth, cl)
    rtyp'     <- compileFRTyp rtyp
    let args1  = argThis cl : args
    args2     <- mapM compileFArg args1
    insts     <- compileFBlock args1 block
    let insts' = insts ++ unreachableMay block
    return $ LFunDef rtyp' name args2 insts'

unreachableMay :: BlockA -> LInsts
unreachableMay block = maybe [LUnreachable] (const []) $
                             lastMay (_bStmts block) >>= (^? _Ret)

compileFName :: Ident -> LIdent
compileFName = _ident

compileFRTyp :: TypeA -> LComp LType
compileFRTyp = compileType

compileFArg :: ArgA -> LComp LArg
compileFArg arg = flip LArg (_ident $ _aIdent arg) <$> compileType (_aTyp arg)

allocArgs :: ArgA -> LComp ()
allocArgs arg = do
    let name = _aIdent arg
    let (name', typ) = (Ident $ "p" ++ _ident name, _aTyp arg)
    ltyp <- compileType typ
    compileAlloca name' typ
    compileStore name' (LTValRef ltyp (LRef $ _ident name))

compileFBlock :: [ArgA] -> BlockA -> LComp LInsts
compileFBlock args block = compileLabel "entry" >>
                           mapM allocArgs args >> compileBlock block >>
                           getInsts <* clearInsts <* resetTemp

compileBlock :: BlockA -> LComp ()
compileBlock = mapM_ compileStmt . _bStmts

compileStmt :: StmtA -> LComp ()
compileStmt = \case
    Empty    _         -> return ()
    BStmt    _ b       -> compileBlock b
    Decl     _ t is_   -> forM_ is_ $ compileDecl t
    Assign   _ lv e    -> compileAssign lv e
    Ret      _ e       -> compileRet e
    VRet     _         -> pushInst LVRet
    Cond     _ c si    -> compileCond     c si
    CondElse _ c si se -> compileCondElse c si se
    While    _ c sw    -> compileWhile    c sw
    For      _ t i e s -> compileFor t i e s
    SExp     _ e       -> void $ compileExpr e

compileStore :: Ident -> LTValRef -> LComp ()
compileStore name tvr = store tvr $ ptrRef (_lTType tvr) (_ident name)

compileDecl :: TypeA -> ItemA -> LComp ()
compileDecl typ item = do
    let name = _iIdent item
    compileAlloca name typ
    let lval = fst $ addTyp (LValueV emptyAnot name []) typ
    compileAssign lval $ fromMaybe (defaultVal typ) (item ^? iExpr)

compileAlloca :: Ident -> TypeA -> LComp ()
compileAlloca name = compileType >=> alloc (_ident name)

compileAssign :: LValueA -> ExprA -> LComp ()
compileAssign lval expr = do
    rhs  <- compileExpr expr
    lhs  <- compileLVal lval
    let LPtr lhst = _lTType lhs
    rhs' <- castIfNeed lhst rhs
    store rhs' lhs

compileRet :: ExprA -> LComp ()
compileRet = compileExpr >$> LRet >=> pushInst

compileCond :: ExprA -> StmtA -> LComp ()
compileCond c si = do
    [_then, _cont] <- newLabels "if" ["then", "cont"]
    compileCondExpr c  _then _cont
    compileCondStmt si _then _cont
    compileLabel             _cont

compileCondElse :: ExprA -> StmtA -> StmtA -> LComp ()
compileCondElse c si se = do
    [_then, _else, _cont] <- newLabels "ifelse" ["then", "else", "cont"]
    compileCondExpr c  _then _else
    compileCondStmt si _then _cont
    compileCondStmt se _else _cont
    compileLabel             _cont

compileWhile :: ExprA -> StmtA -> LComp ()
compileWhile c sw = do
    [_check, _then, _cont] <- newLabels "while" ["check", "then", "cont"]
    compileLabelJmp          _check
    compileCondExpr c  _then _cont
    compileCondStmt sw _then _check
    compileLabel             _cont

compileFor :: TypeA -> Ident -> ExprA -> StmtA -> LComp ()
compileFor typ name eArr si = do
    compileAlloca name typ
    lbt <- compileType typ
    arr <- compileExpr eArr
    l   <- loadLength arr
    basicFor "for" lbt arr l $ load lbt >=> compileStore name .>> compileStmt si

compileCondStmt :: StmtA -> LLabelRef -> LLabelRef -> LComp ()
compileCondStmt stmt _then _cont = xInLabel _then _cont $ compileStmt stmt