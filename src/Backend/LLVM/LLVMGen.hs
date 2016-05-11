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

module Backend.LLVM.LLVMGen (
    -- * Modules
    module Backend.LLVM.Environment,
    module Backend.LLVM.Print,

    -- * Operations
    compileLLVM
) where

import Data.Map ((!))
import Data.Maybe

import Control.Monad

import Control.Lens hiding (Empty, op)

import Utils.Monad

import Common.AST
import Common.Annotations

import Backend.LLVM.Environment
import Backend.LLVM.Print

compileLLVM :: ProgramA -> LComp LLVMCode
compileLLVM = compileLLVMAst >$> printLLVMAst

-- TODO: Refactor
compileLLVMAst :: ProgramA -> LComp LLVMAst
compileLLVMAst = mapM compileFun . _pTopDefs >=>
    \fns -> do
        consts <- getConsts
        return $ LLVMAst consts predefDecls fns

predefDecls :: LFunDecls
predefDecls =
    [LFunDecl LVoid    "printInt"    [intType],
     LFunDecl LVoid    "printDouble" [doubType],
     LFunDecl LVoid    "printString" [strType],
     LFunDecl intType  "readInt"     [],
     LFunDecl doubType "readDouble"  []]

compileFun :: TopDefA -> LComp LFunDef
compileFun (FnDef _ rtyp name args block) = do
    let name' = compileFName name
    let rtyp' = compileFRTyp rtyp
    let args' = compileFArg <$> args
    insts <- compileFBlock $ returnBlock block rtyp
    return $ LFunDef rtyp' name' args' insts

returnBlock :: BlockA -> TypeA ->  BlockA
returnBlock b@(Block a st) rtyp = if null st
    then Block a (st ++ [returnStmt rtyp])
    else case last st of
        Ret  _ _ -> b
        VRet _   -> b
        _        -> Block a (st ++ [returnStmt rtyp])

-- TODO: return unreachable for Ret
returnStmt :: TypeA -> StmtA
returnStmt rtyp = case void rtyp of
    Int  _ -> Ret emptyAnot $ defaultVal rtyp
    Doub _ -> Ret emptyAnot $ defaultVal rtyp
    Bool _ -> Ret emptyAnot $ defaultVal rtyp
    _      -> VRet emptyAnot

compileFName :: Ident -> LIdent
compileFName = _ident

compileFRTyp :: TypeA -> LType
compileFRTyp = compileType

compileFArg :: ArgA -> LArg
compileFArg arg = LArg (compileType $ _aTyp arg) (_ident $ _aIdent arg)

compileFBlock :: BlockA -> LComp LInsts
compileFBlock block = compileLabel "entry" >> compileBlock block >>
                      getInsts <* clearInsts <* resetTemp

compileLabel :: LLabelRef -> LComp ()
compileLabel l = pushInst $ LLabel l

compileLabelJmp :: LLabelRef -> LComp ()
compileLabelJmp l = pushInst (LABr l) >> compileLabel l

xInLabel :: LLabelRef -> LLabelRef -> LComp b -> LComp b
xInLabel _lab _cont x = compileLabel _lab >> x <* pushInst (LABr _cont)

compileBlock :: BlockA -> LComp ()
compileBlock = mapM_ compileStmt . _bStmts

compileStmt :: StmtA -> LComp ()
compileStmt = \case
    Empty    _         -> return ()
    BStmt    _ b       -> compileBlock b
    Decl     _ t is    -> forM_ is $ compileDecl t
    Ass      _ i e     -> compileAss i e
    Incr     a i       -> compileInDeCr a i (Plus emptyAnot)
    Decr     a i       -> compileInDeCr a i (Minus emptyAnot)
    Ret      _ e       -> compileRet e
    VRet     _         -> pushInst LVRet
    Cond     _ c si    -> compileCond     c si
    CondElse _ c si se -> compileCondElse c si se
    While    _ c sw    -> compileWhile    c sw
    SExp     _ e       -> void $ compileExpr e

compileInDeCr :: ASTAnots -> Ident -> AddOpA -> LComp ()
compileInDeCr anots name op = do
    x@(LTValRef t _) <- compileEVar anots name
    temp <- assignTemp t $ switchAdd op t x
        (switchType (LVInt 1) (LVFloat 1) t)
    compileStore name temp

compileStore :: Ident -> LTValRef -> LComp ()
compileStore name tvr = pushInst $ LStore tvr $ LTValRef (LPtr $ _lTType tvr)
                                                         (LRef $ _ident name)

compileDecl :: TypeA -> ItemA -> LComp ()
compileDecl typ item = do
    let name = _iIdent item
    pushInst $ LAssign (_ident name) $ LAlloca (compileType typ)
    compileAss name $ fromMaybe (defaultVal typ) (item ^? iExpr)

compileAss :: Ident -> ExprA -> LComp ()
compileAss name = compileExpr >=> compileStore name

compileRet :: ExprA -> LComp ()
compileRet e = LRet <$> compileExpr e >>= pushInst

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

compileCondExpr :: ExprA -> LLabelRef -> LLabelRef -> LComp ()
compileCondExpr c _then _else = do
    c' <- compileExpr c
    pushInst $ LCBr c' _then _else

compileCondStmt :: StmtA -> LLabelRef -> LLabelRef -> LComp ()
compileCondStmt stmt _then _cont = xInLabel _then _cont $ compileStmt stmt

compileExpr :: ExprA -> LComp LTValRef
compileExpr = \case
    EVar      a i     -> compileEVar a i
    ELitInt   _ v     -> compileLInt   sizeofInt   v
    ELitDoub  _ v     -> compileLFloat sizeofFloat v
    EString   _ v     -> compileCString v
    ELitTrue  _       -> compileLInt   sizeofBool  1
    ELitFalse _       -> compileLInt   sizeofBool  0
    EApp      a i es  -> compileApp a i es
    Neg       _ e     -> compileNeg e
    Not       _ e     -> compileNot e
    EMul      _ l o r -> compileMul o l r
    EAdd      _ l o r -> compileAdd o l r
    ERel      _ l o r -> compileLRel l r o
    EAnd      _ l   r -> compileLBin l r 0 "land"
    EOr       _ l   r -> compileLBin l r 1 "lor"

switchType :: t -> t -> LType -> t
switchType onI onF = \case
    LInt   _  -> onI
    LFloat _  -> onF
    _         -> error "switchType got wrong type."

compileNeg :: ExprA -> LComp LTValRef
compileNeg e = do
    LTValRef t e'  <- compileExpr e
    let (ctor, tvr) = switchType (LSub,  flip LTValRef $ LVInt   0)
                                 (LFSub, flip LTValRef $ LVFloat 0) t
    assignTemp t $ ctor (tvr t) e'

compileBArith :: (LType -> LTValRef -> LValRef -> LExpr) -> ExprA -> ExprA
              -> LComp LTValRef
compileBArith gf l r = do
    l'            <- compileExpr l
    LTValRef t r' <- compileExpr r
    assignTemp t $ gf t l' r'

compileAdd :: AddOpA -> ExprA -> ExprA -> LComp LTValRef
compileAdd = compileBArith . switchAdd

switchAdd :: AddOpA -> LType -> LTValRef -> LValRef -> LExpr
switchAdd op = switchType (compileIAddOp op) (compileFAddOp op)

compileMul :: MulOpA -> ExprA -> ExprA -> LComp LTValRef
compileMul op = compileBArith $ switchType (compileIMulOp op) (compileFMulOp op)

compileIAddOp :: AddOpA -> LTValRef -> LValRef -> LExpr
compileIAddOp = \case
    Plus  _ -> LAdd
    Minus _ -> LSub

compileFAddOp :: AddOpA -> LTValRef -> LValRef -> LExpr
compileFAddOp = \case
    Plus  _ -> LFAdd
    Minus _ -> LFSub

compileIMulOp :: MulOpA -> LTValRef -> LValRef -> LExpr
compileIMulOp = \case
    Times _ -> LMul
    Div   _ -> LDiv
    -- C++ (ISO 2011), https://en.wikipedia.org/wiki/Modulo_operation
    Mod   _ -> LSRem

compileFMulOp :: MulOpA -> LTValRef -> LValRef -> LExpr
compileFMulOp = \case
    Times _ -> LFMul
    Div   _ -> LFDiv
    Mod   _ -> LFRem -- will never happen, but added for completeness.

compileLRel :: ExprA -> ExprA -> RelOpA -> LComp LTValRef
compileLRel l r op = do
    l'            <- compileExpr l
    LTValRef t r' <- compileExpr r
    assignTemp boolType $
        switchType (LICmp . compileRelOpI) (LFCmp . compileRelOpF) t op l' r'

compileRelOpI :: RelOpA -> LICmpOp
compileRelOpI = \case
    LTH _ -> LSlt
    LE  _ -> LSle
    GTH _ -> LSgt
    GE  _ -> LSge
    EQU _ -> LEq
    NE  _ -> LNe

compileRelOpF :: RelOpA -> LFCmpOp
compileRelOpF = \case
    LTH _ -> LFOlt
    LE  _ -> LFOle
    GTH _ -> LFOgt
    GE  _ -> LFOge
    EQU _ -> LFOeq
    NE  _ -> LFOne

compileLBin :: ExprA -> ExprA -> Integer -> String -> LComp LTValRef
compileLBin l r onLHS prefix = do
    lLhs         <- lastLabel
    [lRhs, lEnd] <- newLabels prefix ["rhs", "end"]
    compileCondExpr l lRhs lEnd
    LTValRef _ r' <- xInLabel lRhs lEnd $ compileExpr r
    lRHS' <- lastLabel
    compileLabel lEnd
    assignTemp boolType $
        LPhi boolType [LPhiRef (LVInt onLHS) lLhs, LPhiRef r' lRHS']

compileEVar :: ASTAnots -> Ident -> LComp LTValRef
compileEVar anots name = do
    let typ = compileAnotType anots
    assignTemp typ $ LLoad $ LTValRef (LPtr typ) (LRef $ _ident name)

compileNot :: ExprA -> LComp LTValRef
compileNot = compileExpr >=> assignTemp boolType . flip LXor (LVInt 1)

compileApp :: ASTAnots -> Ident -> [ExprA] -> LComp LTValRef
compileApp anots name es = do
    fr <- LFunRef (_ident name) <$> mapM compileExpr es
    case compileAnotType anots of
        LVoid -> pushInst (LVCall fr) >> return (LTValRef LVoid LNull)
        rtyp  -> assignTemp rtyp $ LCall rtyp fr

assignTemp :: LType -> LExpr -> LComp LTValRef
assignTemp rtyp expr =
    LTValRef rtyp . LRef <$> newTemp <<= pushInst . flip LAssign expr

getType :: ASTAnots -> TypeA
getType anots = let AType typ = anots ! AKType in typ

compileAnotType :: ASTAnots -> LType
compileAnotType = compileType . getType

-- TODO: Use assignTemp
compileCString :: String -> LComp LTValRef
compileCString v = do
    temp <- newTemp
    ref  <- newConstRef "cstring"
    let typ   = LArray (1 + length v) charType
    pushConst $ LConstGlobal ref typ v
    pushInst  $ LAssign temp (strPointer (LPtr typ) ref)
    return    $ LTValRef strType $ LRef temp

compileType :: TypeA -> LType
compileType = \case
    Int      _ -> intType
    Doub     _ -> doubType
    Bool     _ -> boolType
    Void     _ -> LVoid
    ConstStr _ -> strType
    Fun     {} -> error "NOT IMPLEMENTED YET"

boolType, intType, doubType, charType, strType :: LType
boolType = LInt sizeofBool
intType  = LInt sizeofInt
doubType = LFloat sizeofFloat
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