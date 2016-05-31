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
Module      : Backend.LLVM.Types
Description : LType and TypeA related in LLVM backend of the Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

LType and TypeA related in LLVM backend of the Javalette compiler.
-}
{-# LANGUAGE LambdaCase, TupleSections #-}

module Backend.LLVM.Types where

import qualified Data.Map as M

import Control.Arrow

import Control.Lens ((%~))

import Utils.Monad
import Utils.Sizeables

import Common.AST
import Common.Annotations

import Backend.LLVM.Environment

import qualified Frontend.Environment as F

--------------------------------------------------------------------------------
-- LType related:
--------------------------------------------------------------------------------

compileType :: TypeA -> LComp LType
compileType = \case
    Int      _   -> return intType
    Doub     _   -> return doubType
    Bool     _   -> return boolType
    Void     _   -> return LVoid
    a@Array   {} -> aliasFor a
    s@TStruct {} -> aliasFor s
    r@TRef    {} -> aliasFor r
    ConstStr _   -> return strType
    f@Fun     {} -> funType f

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

funType :: TypeA ->  LComp LType
funType f = do
    lrtyp <- compileType (_tfRetTyp f)
    largs <- mapM compileType (_tfArgsTyps f)
    return $ LFunPtr lrtyp largs

--------------------------------------------------------------------------------
-- Aliases:
--------------------------------------------------------------------------------

aliasFor :: TypeA -> LComp LType
aliasFor typ = LPtr . LAlias <$> (getConv typ >>= maybeErr (createAlias typ))

createAlias :: TypeA -> LComp LAliasRef
createAlias typ = case typ of
    Array   {} -> compileType (shrink typ) >>=
                  bindAConv typ . LStruct . (intType:) . return . LArray 0
    TStruct {} -> do
        -- Might have self recursion or even worse: mutual recursion in types.
        -- Thus, create alias first and make it a LPtr to that in aliasFor.
        stAlias <- newAlias <<= insertAConv typ
        _sfType <$$> getStructDef (_tIdent typ) >>= mapM compileType >$> LStruct
            >>= insertAlias stAlias >> return stAlias
    TRef   {}  -> do
        stAlias    <- newAlias <<= insertAConv typ
        cls@(cl:_) <- getClass (_tIdent typ)
        let propt  = _sfType <$> F._ciFieldsDer cl ++ F._ciFields cl
        types     <- mapM compileType propt
        vt        <- makeVTable' stAlias cls
        insertAlias stAlias (LStruct $ vt ++ types)
        return stAlias
    _          -> error "createAlias: should be handled by compileType."

--------------------------------------------------------------------------------
-- VTable:
--------------------------------------------------------------------------------

makeVTable' :: LAliasRef -> [F.ClassInfo] -> LComp [LType]
makeVTable' alias cls = do
    let virtMs = filter isVirt $ getMethodsUniq cls
    if null virtMs then return [] else makeVTable alias virtMs

makeVTable :: LAliasRef -> [(FnDefA, F.ClassInfo)] -> LComp [LType]
makeVTable alias virts = do
    let (typs, ids)  = unzip $ (methToFnPtr &&& LRef . nameMethod)
                            <$> virts
    fptrs     <- mapM compileType typs
    let vtType = nameVTableT alias
    insertAlias vtType (LStruct fptrs)
    let vtdata = LTValRef (LAlias vtType) $ LVArray $ zipWith LTValRef fptrs ids
    pushConst $ LConstGlobal (nameVTableD alias) vtdata
    return [LPtr $ LAlias vtType]

isVirt :: (FnDefA, F.ClassInfo) -> Bool
isVirt (fn, _) = extractVirt fn

nameVTableD, nameVTableT, nameVTable :: String -> String
nameVTableD = (++ "_data") . nameVTable
nameVTableT = (++ "_type") . nameVTable
nameVTable  = (++ "_vtable")

hasVirtual :: [F.ClassInfo] -> Bool
hasVirtual cls = or $ extractVirt <$> (cls >>= M.elems . F._ciMethods)

makeCLTyp :: F.ClassInfo -> TypeA
makeCLTyp cl = appConcrete $ flip TRef (F._ciIdent cl)

argThis :: F.ClassInfo -> ArgA
argThis cl = Arg emptyAnot (makeCLTyp cl) (Ident "this")

methToFnPtr :: (FnDefA, F.ClassInfo) -> TypeA
methToFnPtr (fn, cl) = toFnPtr $ fArgs %~ (argThis cl:) $ fn

toFnPtr :: FnDefA -> TypeA
toFnPtr fn = let F.FunSig args ret = F.toFnSig fn in Fun emptyAnot ret args

nameMethod' :: (Integer, (FnDefA, F.ClassInfo)) -> FnDefA
nameMethod' (_, x@(fn, _)) = fn { _fIdent = Ident $ nameMethod x }

nameMethod :: (FnDefA, F.ClassInfo) -> LIdent
nameMethod (fn, cl) = _ident (F._ciIdent cl) ++ "__" ++ _ident (_fIdent fn)