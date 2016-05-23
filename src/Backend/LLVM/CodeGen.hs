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
import Data.Maybe

import Control.Monad

import Control.Lens hiding (Empty, op, uncons, to)

import Utils.Pointless
import Utils.Monad
import Utils.Sizeables

import Common.AST
import Common.Annotations

import Backend.LLVM.Environment
import Backend.LLVM.Print

u = undefined

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
    let name'  = compileFName name
    rtyp'     <- compileFRTyp rtyp
    args'     <- mapM compileFArg args
    insts     <- compileFBlock args block
    let insts' = insts ++ unreachableMay block
    return $ LFunDef rtyp' name' args' insts'

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
compileStore name tvr = store tvr $ LTValRef (LPtr $ _lTType tvr)
                                             (LRef $ _ident name)

compileDecl :: TypeA -> ItemA -> LComp ()
compileDecl typ item = do
    let name = _iIdent item
    compileAlloca name typ
    compileAss name $ fromMaybe (defaultVal typ) (item ^? iExpr)

compileAlloca :: Ident -> TypeA -> LComp ()
compileAlloca name = compileType >=> alloc (_ident name)

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

compileExpr :: ExprA -> LComp LTValRef
compileExpr = \case
    ENew      a _ ds  -> compileENew a ds
    EVar      a i _   -> compileEVar a i
    Length    _ e     -> compileLength e
    ELitInt   _ v     -> return $ intTVR  v
    ELitDoub  _ v     -> return $ doubTVR v
    EString   _ v     -> compileCString   v
    ELitTrue  _       -> return ltrue
    ELitFalse _       -> return lfalse
    ECastNull _ t     -> compileCastNull t
    EApp      a i es  -> compileApp a i es
    Neg       _ e     -> compileNeg e
    Not       _ e     -> compileNot e
    EMul      _ l o r -> compileMul  o l r
    EAdd      _ l o r -> compileAdd  o l r
    ERel      _ l o r -> compileLRel o l r
    EAnd      _ l   r -> compileLBin l r 0 "land"
    EOr       _ l   r -> compileLBin l r 1 "lor"

compileCastNull :: TypeA -> LComp LTValRef
compileCastNull = compileType >$> flip LTValRef LNull

switchType :: t -> t -> LType -> t
switchType onI onF = \case
    LInt   _  -> onI
    LFloat _  -> onF
    _         -> error "switchType got wrong type."

compileNeg :: ExprA -> LComp LTValRef
compileNeg e = do
    LTValRef t e' <- compileExpr e
    assignTemp t $ switchType (LSub izero) (LFSub dzero) t e'

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
    assignBool $ LPhi boolType [LPhiRef (LVInt onLHS) lLhs, LPhiRef r' lRhs']

compileENew :: ASTAnots -> [DimEA] -> LComp LTValRef
compileENew = compileNew . getType

compileNew :: TypeA -> [DimEA] -> LComp LTValRef
compileNew bt  []     = compileType bt >>= \lbt -> salloc lbt return lbt
compileNew typ (d:ds) = do
    (bt, lbt) <- fkeep compileType $ shrink typ
    l         <- compileExpr $ _deExpr d
    (compileType typ >>= salloc lbt (imul l >=> iadd lenSize)) <<= setLength l
        <<= \newed -> unless (null ds) $ basicFor "newSubArr" lbt newed l $
            (compileNew bt ds >>=) . flip store

salloc :: LType -> (LTValRef -> LComp LTValRef) -> LType -> LComp LTValRef
salloc et sc rt = compileSizeof et >>= sc >>= flip compileCalloc ione
                                          >>= bitcast rt

compileFor :: TypeA -> Ident -> ExprA -> StmtA -> LComp ()
compileFor typ name eArr si = do
    compileAlloca name typ
    lbt <- compileType typ
    arr <- compileExpr eArr
    l   <- loadLength arr
    basicFor "for" lbt arr l $ load lbt >=> compileStore name .>> compileStmt si

compileEVar :: ASTAnots -> Ident -> LComp LTValRef
compileEVar anots name = do
    typ   <- compileAnotType anots
    let vs = case getVS anots of VSLocal -> ""; VSArg -> "p"
    load typ $ LTValRef (LPtr typ) (LRef $ vs ++ _ident name)

compileLength :: ExprA -> LComp LTValRef
compileLength = compileExpr >=> loadLength

compileNot :: ExprA -> LComp LTValRef
compileNot = compileExpr >=> assignBool . flip LXor (LVInt 1)

compileApp :: ASTAnots -> Ident -> [ExprA] -> LComp LTValRef
compileApp anots name es = do
    fr <- LFunRef (_ident name) <$> mapM compileExpr es
    compileAnotType anots >>= \case
        LVoid -> vcall fr
        rtyp  -> assignCall rtyp fr

compileCString :: String -> LComp LTValRef
compileCString v = do
    ref  <- newConstRef "cstring"
    let typ   = LArray (1 + genericLength v) charType
    pushConst $ LConstGlobal ref typ v
    assignTemp strType $ strPointer (LPtr typ) ref

strPointer :: LType -> LIdent -> LExpr
strPointer t = deref [izero] . LTValRef t . LConst

--------------------------------------------------------------------------------
-- Pointers, Structs, Arrays:
--------------------------------------------------------------------------------

compileSizeof :: LType -> LComp LTValRef
compileSizeof t = assignPtr t (LGElemPtr (LTValRef (LPtr t) LNull) ione [])
                  >>= ptrToInt

compileCalloc :: LTValRef -> LTValRef -> LComp LTValRef
compileCalloc n sizeof = assignCall bytePType $ LFunRef "calloc" [n, sizeof]

lenSize :: LTValRef
lenSize = intTVR $ sizeofInt `div` 8

loadLength :: LTValRef -> LComp LTValRef
loadLength = lengthRef >=> loadi

setLength :: LTValRef -> LTValRef -> LComp ()
setLength len = lengthRef >=> store len

lengthRef :: LTValRef -> LComp LTValRef
lengthRef = assignIntP . deref [izero]

basicFor :: LLabelRef -> LType -> LTValRef -> LTValRef
        -> (LTValRef -> LComp ()) -> LComp ()
basicFor prefix lbt arr len handler = do
    [_check, _then, _cont] <- newLabels prefix ["check", "then", "cont"]
    -- set: i = 0
    i     <- alloci <<= store izero
    compileLabelJmp _check
    -- check: i < arr.length
    iload <- loadi i
    assignBool (LICmp LSlt iload $ _lTVRef len) >>= condBr _then _cont
    -- load: arr[i] + body + store: i++
    xInLabel _then _check $
        assignTemp (LPtr lbt) (deref [ione, iload] arr) >>= handler >>
        iadd iload ione >>= flip store i
    compileLabel _cont

deref :: LTValRefs -> LTValRef -> LExpr
deref = flip $ flip LGElemPtr izero

--------------------------------------------------------------------------------
-- Branching, Labels:
--------------------------------------------------------------------------------

compileCondExpr :: ExprA -> LLabelRef -> LLabelRef -> LComp ()
compileCondExpr c _then _else = compileExpr c >>= condBr _then _else

compileCondStmt :: StmtA -> LLabelRef -> LLabelRef -> LComp ()
compileCondStmt stmt _then _cont = xInLabel _then _cont $ compileStmt stmt

compileLabel :: LLabelRef -> LComp ()
compileLabel l = pushInst $ LLabel l

compileLabelJmp :: LLabelRef -> LComp ()
compileLabelJmp l = aBr l >> compileLabel l

xInLabel :: LLabelRef -> LLabelRef -> LComp b -> LComp b
xInLabel _lab _cont x = compileLabel _lab >> x <* aBr _cont

condBr :: LLabelRef -> LLabelRef -> LTValRef -> LComp ()
condBr _then _else r = pushInst $ LCBr r _then _else

aBr :: LLabelRef -> LComp ()
aBr = pushInst . LABr

--------------------------------------------------------------------------------
-- Constant values + Constructors:
--------------------------------------------------------------------------------

izero, ione, dzero, ltrue, lfalse :: LTValRef
izero  = intTVR 0
ione   = intTVR 1
dzero  = doubTVR 0
ltrue  = boolTVR True
lfalse = boolTVR False

intTVR :: Integer -> LTValRef
intTVR = LTValRef intType . LVInt

boolTVR :: Bool -> LTValRef
boolTVR = LTValRef boolType . LVInt . toInteger . fromEnum

doubTVR :: Double -> LTValRef
doubTVR = LTValRef doubType . LVFloat

--------------------------------------------------------------------------------
-- Basic operations:
--------------------------------------------------------------------------------

iadd, imul :: LTValRef -> LTValRef -> LComp LTValRef
iadd a (LTValRef _ b) = assignInt $ LAdd a b
imul a (LTValRef _ b) = assignInt $ LMul a b

ptrToInt :: LTValRef -> LComp LTValRef
ptrToInt = assignInt . flip LPtrToInt intType

bitcast :: LType -> LTValRef -> LComp LTValRef
bitcast to ptr = assignTemp to (LBitcast ptr to)

alloci :: LComp LTValRef
alloci = assignIntP $ LAlloca intType

loadi :: LTValRef -> LComp LTValRef
loadi = assignInt . LLoad

load :: LType -> LTValRef -> LComp LTValRef
load typ = assignTemp typ . LLoad

assignBool, assignInt, assignIntP :: LExpr -> LComp LTValRef
assignBool = assignTemp boolType
assignInt  = assignTemp intType
assignIntP = assignPtr  intType

assignPtr :: LType -> LExpr -> LComp LTValRef
assignPtr = assignTemp . LPtr

assignCall :: LType -> LFunRef -> LComp LTValRef
assignCall rtyp = assignTemp rtyp . LCall rtyp

vcall :: LFunRef -> LComp LTValRef
vcall fr = pushInst (LVCall fr) >> return (LTValRef LVoid LNull)

assignTemp :: LType -> LExpr -> LComp LTValRef
assignTemp rtyp expr = LTValRef rtyp . LRef <$> newTemp <<= flip assignE expr

alloc :: LIdent -> LType -> LComp ()
alloc n = assignE n . LAlloca

assignE :: LIdent -> LExpr -> LComp ()
assignE n = pushInst . LAssign n

store :: LTValRef -> LTValRef -> LComp ()
store = pushInst .| LStore

--------------------------------------------------------------------------------
-- LType related:
--------------------------------------------------------------------------------

compileType :: TypeA -> LComp LType
compileType = \case
    Int      _     -> return intType
    Doub     _     -> return doubType
    Bool     _     -> return boolType
    Void     _     -> return LVoid
    arr@(Array {}) -> aliasFor arr
    ConstStr _     -> return strType
    Fun      {}    -> error "compileType Fun not implemented."

boolType, intType, doubType, charType, strType, byteType, bytePType :: LType
boolType  = LInt sizeofBool
intType   = LInt sizeofInt
doubType  = LFloat sizeofFloat
charType  = LInt sizeofChar
strType   = LPtr charType
byteType  = LInt sizeofByte
bytePType = LPtr byteType

sizeofBool, sizeofChar, sizeofByte, sizeofInt, sizeofFloat :: Integer
sizeofBool  = 1
sizeofChar  = 8
sizeofByte  = 8
sizeofInt   = 32
sizeofFloat = 64

compileAnotType :: ASTAnots -> LComp LType
compileAnotType = compileType . getType

--------------------------------------------------------------------------------
-- Aliases:
--------------------------------------------------------------------------------

aliasFor :: TypeA -> LComp LType
aliasFor typ = getConv typ >>= maybe (createAlias typ) (return . LAlias)

createAlias :: TypeA -> LComp LType
createAlias typ = if notArray typ then compileType typ else
    (>>=) (aliasFor $ shrink typ) $
    bindAlias . LStruct . (intType:) . return . LArray 0 >$> LAlias >=>
    bindAConv typ . LPtr >$> LAlias