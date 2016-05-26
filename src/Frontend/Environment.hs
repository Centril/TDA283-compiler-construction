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
Module      : Frontend.Environment
Description : Operating environment of type checker in Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Operating environment of type checker in Javalette compiler.
-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Environment (
    -- * Modules
    module X,

    -- * Types
    TCComp, IOTCComp,
    TCEnv(..), Context, Contexts, Var(..),
    FunSig(..), FunId(..), FnSigMap,

    -- * Operations
    initialTCEnv, functions, contexts, toFunId,
    lookupVar', lookupFun', currUnused,
    extendVar', extendFun',
    extendTypeDef, lookupTypeName,
    extendStruct
) where

import Prelude hiding (lookup)

import Data.Map (Map, empty, lookup, insert, updateLookupWithKey, elems)

import Control.Monad

import Control.Lens hiding (Context, contexts, uncons)

import Utils.Monad
import Utils.Foldable

import Common.StateOps as X
import Common.Computation as X
import Common.AST
import Common.Annotations as X

--------------------------------------------------------------------------------
-- Scopes / Contexts:
--------------------------------------------------------------------------------

-- | 'Context': A context for a scope, map from variables -> types.
type Context = Map Ident Var

-- | 'Contexts': List of 'Context'
type Contexts = [Context]

-- | 'Var': a variable specified by its 'Ident', 'Type', 'VarSource'.
data Var = Var { vident  :: Ident,     vtype :: TypeA,
                 vsource :: VarSource, vuses :: Integer }
    deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- Function Signatures:
--------------------------------------------------------------------------------

-- | 'FunSig': Signature of a function,
-- argument list (types) followed by return type.
data FunSig = FunSig { targs :: [TypeA], tret :: TypeA}
    deriving (Eq, Show, Read)

-- | 'FnId': Signature of a function ('FunSig') + 'Ident'.
data FunId = FunId { fident :: Ident, fsig :: FunSig}
    deriving (Eq, Show, Read)

-- | 'FnSigMap': Map of function identifiers -> signatures.
type FnSigMap = Map Ident FunSig

toFunId :: (String, ([TypeA], TypeA)) -> FunId
toFunId (name, sig) = FunId (Ident name) $ uncurry FunSig sig

--------------------------------------------------------------------------------
-- Type names:
--------------------------------------------------------------------------------

-- | 'ReservedTypes': Map of type names -> actual types.
type ReservedTypes = Map Ident TypeA

--------------------------------------------------------------------------------
-- Structs:
--------------------------------------------------------------------------------

type StructDefMap = Map Ident [SFieldA]

--------------------------------------------------------------------------------
-- Operating Environment:
--------------------------------------------------------------------------------

-- | 'TCEnv': The operating environment of an 'TCComp' computation.
data TCEnv = TCEnv {
      _reserved  :: ReservedTypes
    , _structs   :: StructDefMap
    , _functions :: FnSigMap  -- ^ Map of ident -> function signatures.
    , _contexts  :: Contexts } -- ^ Stack of contexts.
    deriving (Eq, Show, Read)

makeLenses ''TCEnv

-- | 'initialTCEnv': The initial empty typechecker environment.
initialTCEnv :: TCEnv
initialTCEnv = TCEnv empty empty empty [empty]

--------------------------------------------------------------------------------
-- Computations in compiler:
--------------------------------------------------------------------------------

-- | 'IOTCComp': A computation in typechecker gifted with IO powers, use wisely.
type IOTCComp a = IOComp TCEnv a

-- | 'TCComp': A computation in typechecker using environment 'TCEnv'.
type TCComp a = Comp TCEnv a

--------------------------------------------------------------------------------
-- Environment operations:
--------------------------------------------------------------------------------

checkNotReserved :: (Ident -> TypeA -> TCComp ()) -> Ident -> TCComp ()
checkNotReserved onErr name = do
    rts <- use reserved
    maybe (return ()) (onErr name) (lookup name rts)

extendStruct :: (Ident -> TypeA -> TCComp ())
             -> TypeA -> Ident -> [SFieldA] -> TCComp ()
extendStruct onErr typ name fields = do
    checkNotReserved onErr name
    reserved %= insert name typ
    structs  %= insert name fields

-- | 'extendTypeDef': Extends current list of typedefs with the one given,
-- or fails with onErr if the type name already exists.
extendTypeDef :: (Ident -> TypeA -> TCComp ()) -> Ident -> TypeA -> TCComp ()
extendTypeDef onErr alias typ = do
    checkNotReserved onErr alias
    reserved %= insert alias typ

-- | 'lookupTypeName': If name is bound, returns the corresponding type.
-- Fails with an error otherwise.
lookupTypeName :: (Ident -> TCComp TypeA) -> Ident -> TCComp TypeA
lookupTypeName = lookupX reserved

-- | 'currUnused': yields all unused variables in most local scope.
currUnused :: TCComp [Var]
currUnused = filter ((== 0) . vuses) <$> uses contexts (elems . head)

-- | 'lookupVar': If var exists in any scope in the 'Contexts', the 'Var' of
-- the identifier is 'return':ed, otherwise onErr is given the (name = 'Ident').
lookupVar' :: (Ident -> TCComp Var) -> Ident -> TCComp Var
lookupVar' onErr name = contexts %%= mfindU (ctxLookup name) >>=
                        maybeErr (onErr name)

ctxLookup :: Ident -> Context -> Maybe (Var, Context)
ctxLookup name ctx = case updateLookupWithKey incUses name ctx of
    (Just var, ctx') -> Just (var, ctx')
    (Nothing , _   ) -> Nothing
    where incUses _ var = Just $ var { vuses = 1 + vuses var }

-- | 'lookupFun': If function with given identifier exists, the 'FunSig' of it
-- is 'return':ed, otherwise, onErr is given the (fun = 'Ident').
lookupFun' :: (Ident -> TCComp FunSig) -> Ident -> TCComp FunSig
lookupFun' = lookupX functions

-- | 'extendVar': Extends the current scope with the given variable with name
-- and it's additional information. If variable exists, onError is used.
extendVar' :: (Ident -> TCComp ()) -> Var -> TCComp ()
extendVar' onErr var = do
    let name = vident var
    (c : ctxs) <- use contexts
    maybe (contexts .= insert name var c:ctxs)
          (const $ onErr name)
          (lookup name c)

-- | 'extendVar': Extends the accumulated function signatures with the given
-- signature as 'FnId'. If function exists, onError is used.
extendFun' :: (Ident -> TCComp ()) -> FunId -> TCComp ()
extendFun' onErr (FunId name sig) = do
    funs <- use functions
    maybe (functions .= insert name sig funs)
          (const $ onErr name)
          (lookup name funs)

lookupX :: Ord k => LensLike' (Const (Maybe b)) TCEnv (Map k b)
        -> (k -> TCComp b) -> k -> TCComp b
lookupX store onErr k = uses store (lookup k) >>= maybeErr (onErr k)