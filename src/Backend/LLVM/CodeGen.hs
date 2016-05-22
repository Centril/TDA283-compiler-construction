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
{-# LANGUAGE LambdaCase #-}

module Backend.LLVM.CodeGen (
    -- * Modules
    module Backend.LLVM.Environment,
    module Backend.LLVM.Print,

    -- * Operations
    compileLLVM
) where

import Safe

import Data.List
import Data.Tuple
import Data.Map ((!))
import Data.Maybe

import Control.Monad

import Control.Lens hiding (Empty, op)

import Utils.Shallow
import Utils.Monad
import Utils.Sizeables

import Common.AST
import Common.Annotations

import Backend.LLVM.Environment
import Backend.LLVM.Print

compileLLVM :: ProgramA -> LComp LLVMCode
compileLLVM = compileLLVMAst >$> printLLVMAst

compileLLVMAst :: ProgramA -> LComp LLVMAst
compileLLVMAst = mapM compileFun . _pTopDefs >=>
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

compileFun :: TopDefA -> LComp LFunDef
compileFun (FnDef _ rtyp name args block) = do
    let name' = compileFName name
    let rtyp' = compileFRTyp rtyp
    let args' = compileFArg <$> args
    insts <- compileFBlock args block
    let insts' = insts ++ unreachableMay block
    return $ LFunDef rtyp' name' args' insts'

unreachableMay :: BlockA -> LInsts
unreachableMay block = maybe [LUnreachable] (const []) $
                             lastMay (_bStmts block) >>= (^? _Ret)

compileFName :: Ident -> LIdent
compileFName = _ident

compileFRTyp :: TypeA -> LType
compileFRTyp = compileType

compileFArg :: ArgA -> LArg
compileFArg arg = LArg (compileType $ _aTyp arg) (_ident $ _aIdent arg)

allocArgs :: ArgA -> LComp ()
allocArgs arg = do
    let name = _aIdent arg
    let (name', typ) = (Ident $ "p" ++ _ident name, _aTyp arg)
    compileAlloca name' typ
    compileStore name' (LTValRef (compileType typ) (LRef $ _ident name))

compileFBlock :: [ArgA] -> BlockA -> LComp LInsts
compileFBlock args block = compileLabel "entry" >>
                           mapM allocArgs args >> compileBlock block >>
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
    AssArr   _ i d e   -> compileAssArr i d e
    Incr     a i       -> compileInDeCr a i (Plus emptyAnot)
    Decr     a i       -> compileInDeCr a i (Minus emptyAnot)
    Ret      _ e       -> compileRet e
    VRet     _         -> pushInst LVRet
    Cond     _ c si    -> compileCond     c si
    CondElse _ c si se -> compileCondElse c si se
    While    _ c sw    -> compileWhile    c sw
    For      _ t i e s -> compileFor t i e s
    SExp     _ e       -> void $ compileExpr e

compileInDeCr :: ASTAnots -> Ident -> AddOpA -> LComp ()
compileInDeCr anots name op = do
    x@(LTValRef t _) <- compileEVar anots name
    assignTemp t (switchAdd op t x $ switchType (LVInt 1) (LVFloat 1) t)
        >>= compileStore name

compileStore :: Ident -> LTValRef -> LComp ()
compileStore name tvr = pushInst $ LStore tvr $ LTValRef (LPtr $ _lTType tvr)
                                                         (LRef $ _ident name)

compileDecl :: TypeA -> ItemA -> LComp ()
compileDecl typ item = do
    let name = _iIdent item
    compileAlloca name typ
    compileAss name $ fromMaybe (defaultVal typ) (item ^? iExpr)

compileAlloca :: Ident -> TypeA -> LComp ()
compileAlloca name typ = pushInst $ LAssign (_ident name)
                                  $ LAlloca (compileType typ)

compileAss :: Ident -> ExprA -> LComp ()
compileAss = compileExpr >=?> compileStore

-- TODO: Implement
compileAssArr :: Ident -> [DimEA] -> ExprA -> LComp ()
compileAssArr = error "compileAssArr undefined"

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

pushLCBr :: LTValRef -> LLabelRef -> LLabelRef -> LComp ()
pushLCBr r _then _else = pushInst $ LCBr r _then _else

compileCondExpr :: ExprA -> LLabelRef -> LLabelRef -> LComp ()
compileCondExpr c _then _else = compileExpr c >>= \e -> pushLCBr e _then _else

compileCondStmt :: StmtA -> LLabelRef -> LLabelRef -> LComp ()
compileCondStmt stmt _then _cont = xInLabel _then _cont $ compileStmt stmt

compileExpr :: ExprA -> LComp LTValRef
compileExpr = \case
    ENew      a t ds  -> compileENew a t ds
    EVar      a i _   -> compileEVar a i
    Length    _ e     -> compileLength e
    ELitInt   _ v     -> compileLInt   sizeofInt   v
    ELitDoub  _ v     -> compileLFloat sizeofFloat v
    EString   _ v     -> compileCString v
    ELitTrue  _       -> compileLInt   sizeofBool  1
    ELitFalse _       -> compileLInt   sizeofBool  0
    EApp      a i es  -> compileApp a i es
    Neg       _ e     -> compileNeg e
    Not       _ e     -> compileNot e
    EMul      _ l o r -> compileMul  o l r
    EAdd      _ l o r -> compileAdd  o l r
    ERel      _ l o r -> compileLRel o l r
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

compileBArith :: (LType -> LType)
              -> (LType -> LTValRef -> LValRef -> LExpr) -> ExprA -> ExprA
              -> LComp LTValRef
compileBArith th gf l r = do
    l'            <- compileExpr l
    LTValRef t r' <- compileExpr r
    assignTemp (th t) $ gf t l' r'

compileAdd :: AddOpA -> ExprA -> ExprA -> LComp LTValRef
compileAdd = compileBArith id . switchAdd

switchAdd :: AddOpA -> LType -> LTValRef -> LValRef -> LExpr
switchAdd op = switchType (compileIAddOp op) (compileFAddOp op)

compileMul :: MulOpA -> ExprA -> ExprA -> LComp LTValRef
compileMul op = compileBArith id $
                switchType (compileIMulOp op) (compileFMulOp op)

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

compileLRel :: RelOpA -> ExprA -> ExprA -> LComp LTValRef
compileLRel = compileBArith (const boolType) .
    flip (switchType (LICmp . compileRelOpI) (LFCmp . compileRelOpF))

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
    [lRhs, lEnd] <- newLabels prefix ["rhs", "end"]
    uncurry (compileCondExpr l) $
        (if onLHS == 0 then id else swap) (lRhs, lEnd)
    lLhs          <- lastLabel
    LTValRef _ r' <- xInLabel lRhs lEnd $ compileExpr r
    lRhs'         <- lastLabel
    compileLabel lEnd
    assignTemp boolType $
        LPhi boolType [LPhiRef (LVInt onLHS) lLhs, LPhiRef r' lRhs']

aliasFor :: TypeA -> LComp LType
aliasFor typ = getConv typ >>= maybe (createAlias typ) (return . LAlias)

createAlias :: TypeA -> LComp LType
createAlias = \case
    arr@(Array {}) -> do
        smaller <- aliasFor $ shrink arr
        struct  <- LAlias <$> bindAlias (LStruct [intType, LArray 0 smaller])
        LAlias <$> bindAConv arr (LPtr struct)
    x              -> return $ compileType x

compileSizeof :: LType -> LComp LTValRef
compileSizeof typ = let typ' = LPtr typ
                    in assignTemp typ' (LGElemPtr (LTValRef typ' LNull) ione [])
                        >>= assignInt . flip LPtrToInt intType

compileCalloc :: LTValRef -> LTValRef -> LComp LTValRef
compileCalloc nElems sizeof =
    assignTemp bytePType $ LCall bytePType $ LFunRef "calloc" [nElems, sizeof]

add, mul :: LTValRef -> LTValRef -> LComp LTValRef
add a (LTValRef _ b) = assignInt $ LAdd a b
mul a (LTValRef _ b) = assignInt $ LMul a b

lenSize :: LTValRef
lenSize = intTVR $ sizeofInt `div` 8

compileNewSize :: LType -> [LTValRef] -> LComp LTValRef
compileNewSize bt = \case
    [ ] -> compileSizeof bt
    [x] -> compileSizeof bt >>= mul x >>= add lenSize
    xs -> do {- Logic is: x = a + b + c; sizeof = (x + d) * sb + (x + 1) * si -}
        btSize <- compileSizeof bt
        let (rhead:rtail) = reverse xs
        t0 <- foldl1M add rtail
        t1 <- add rhead t0 >>= mul btSize
        t2 <- add ione  t0 >>= mul lenSize
        add t1 t2

compileENew :: ASTAnots -> TypeA -> [DimEA] -> LComp LTValRef
compileENew anots bt dimes = do
    accs <- mapM (compileExpr . _deExpr) dimes
    t1   <- compileNewSize (compileType bt) accs >>= flip compileCalloc ione
    (typ, ltyp) <- fkeep aliasFor $ getType anots
    assignTemp ltyp (LBitcast t1 ltyp) <<= initializeLengths typ accs

initializeLengths :: TypeA -> LTValRefs -> LTValRef -> LComp ()
initializeLengths typ accs arr = case accs of
    [ ]    -> return ()
    [x]    -> initLen x
    (x:xs) -> forLoop "init.length" typ arr (initLen x >> return x) (handler xs)
    where initLen len = lengthRef arr >>= pushInst . LStore len
          handler xs ltyp smaller _elemp = assignTemp ltyp (LLoad _elemp) >>=
                                           initializeLengths smaller xs

lengthRef :: LTValRef -> LComp LTValRef
lengthRef arr = assignIntP $ LGElemPtr arr izero [izero]

compileFor :: TypeA -> Ident -> ExprA -> StmtA -> LComp ()
compileFor typ name eArr si = do
    let eArrT = getType $ extract eArr
    compileAlloca name typ
    lArr <- compileExpr eArr
    forLoop "for" eArrT lArr (lengthRef lArr >>= assignInt . LLoad) $
        \ltyp _ _elemp -> do
            assignTemp ltyp (LLoad _elemp) >>= compileStore name
            compileStmt si

forLoop :: LLabelRef -> TypeA -> LTValRef -> LComp LTValRef
        -> (LType -> TypeA -> LTValRef -> LComp ()) -> LComp ()
forLoop prefix typ arr lengthInit handler = do
    [_check, _then, _cont] <- newLabels prefix ["check", "then", "cont"]
    -- set: i = 0, load: arr.length
    l     <- lengthInit
    i     <- assignIntP (LAlloca intType) <<= pushInst . LStore izero
    compileLabelJmp _check
    -- check: i < arr.length
    iload <- assignInt $ LLoad i
    ref   <- assignTemp boolType (LICmp LSlt iload $ _lTVRef l)
    pushLCBr ref _then _cont
    -- load: arr[i] + body:
    xInLabel _then _check $ do
        (smaller, ltyp) <- fkeep aliasFor (shrink typ)
        assignTemp (LPtr ltyp) (LGElemPtr arr izero [ione, iload])
            >>= handler ltyp smaller
        -- store: i++
        add iload ione >>= pushInst . flip LStore i
    compileLabel _cont

compileEVar :: ASTAnots -> Ident -> LComp LTValRef
compileEVar anots name = do
    let typ = compileAnotType anots
    let vs = case getVS anots of VSLocal -> ""; VSArg -> "p"
    assignTemp typ $ LLoad $ LTValRef (LPtr typ) (LRef $ vs ++ _ident name)

compileLength :: ExprA -> LComp LTValRef
compileLength = compileExpr >=> lengthRef >=> assignInt . LLoad

compileNot :: ExprA -> LComp LTValRef
compileNot = compileExpr >=> assignTemp boolType . flip LXor (LVInt 1)

compileApp :: ASTAnots -> Ident -> [ExprA] -> LComp LTValRef
compileApp anots name es = do
    fr <- LFunRef (_ident name) <$> mapM compileExpr es
    case compileAnotType anots of
        LVoid -> pushInst (LVCall fr) >> return (LTValRef LVoid LNull)
        rtyp  -> assignTemp rtyp $ LCall rtyp fr

assignInt :: LExpr -> LComp LTValRef
assignInt = assignTemp intType

assignIntP :: LExpr -> LComp LTValRef
assignIntP = assignTemp $ LPtr intType

assignTemp :: LType -> LExpr -> LComp LTValRef
assignTemp rtyp expr =
    LTValRef rtyp . LRef <$> newTemp <<= pushInst . flip LAssign expr

getVS :: ASTAnots -> VarSource
getVS anots = let AVarSource vs = anots ! AKVarSource in vs

getType :: ASTAnots -> TypeA
getType anots = let AType typ = anots ! AKType in typ

compileAnotType :: ASTAnots -> LType
compileAnotType = compileType . getType

compileCString :: String -> LComp LTValRef
compileCString v = do
    ref  <- newConstRef "cstring"
    let typ   = LArray (1 + genericLength v) charType
    pushConst $ LConstGlobal ref typ v
    assignTemp strType $ strPointer (LPtr typ) ref

compileType :: TypeA -> LType
compileType = \case
    Int      _     -> intType
    Doub     _     -> doubType
    Bool     _     -> boolType
    Void     _     -> LVoid
    Array    _ _ _ -> arrayType
    ConstStr _     -> strType
    Fun      {}    -> error "NOT IMPLEMENTED YET"

boolType, intType, doubType, charType, strType, byteType, bytePType :: LType
boolType  = LInt sizeofBool
intType   = LInt sizeofInt
doubType  = LFloat sizeofFloat
charType  = LInt sizeofChar
strType   = LPtr charType
byteType  = LInt sizeofByte
bytePType = LPtr byteType

-- TODO: Implement
arrayType :: LType
arrayType = error "arrayType undefined"

intTVR :: Integer -> LTValRef
intTVR = LTValRef intType . LVInt

izero, ione :: LTValRef
izero = intTVR 0
ione  = intTVR 1

strPointer :: LType -> LIdent -> LExpr
strPointer t i = LGElemPtr (LTValRef t $ LConst i) izero [izero]

compileLInt :: Integer -> Integer -> LComp LTValRef
compileLInt s v = return $ LTValRef (LInt s) (LVInt v)

compileLFloat :: Integer -> Double -> LComp LTValRef
compileLFloat s v = return $ LTValRef (LFloat s) (LVFloat v)

sizeofBool, sizeofChar, sizeofByte, sizeofInt, sizeofFloat :: Integer
sizeofBool  = 1
sizeofChar  = 8
sizeofByte  = 8
sizeofInt   = 32
sizeofFloat = 64