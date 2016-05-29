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
    ClassInfo (..), ClassDAG, ClassGraph, ClassToCGNode, CGNode, CGEdge,

    -- * Operations
    initialTCEnv, functions, contexts, toFunId,
    lookupVar', currUnused, extendVar',
    lookupFun', extendFun',
    extendTypeName, lookupTypeName,
    extendStruct, lookupStruct, structs,
    ciIdent, ciHierarchy, ciMethods, ciFields,
    lookupClass, lookupMethod, classGraph
) where

import Prelude hiding (lookup)

import Data.List (find)
import Data.Maybe
import qualified Data.Graph.Inductive as G
import qualified Utils.GraphFlip as GF
import Data.Map (Map, empty, lookup, insert, updateLookupWithKey, elems)

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
-- Type names: Structs, Classes, Typedefs:
--------------------------------------------------------------------------------

-- | 'ReservedTypes': Map of type names -> actual types.
type ReservedTypes = Map Ident TypeA

-- | 'StructDefMap': Map from struct type names to its fields.
type StructDefMap = Map Ident [SFieldA]

-- | 'ClassInfo': considerably simplified way of describing 'ClassDef'.
data ClassInfo = ClassInfo {
      _ciIdent     :: Ident       -- ^ Name of class.
    , _ciFields    :: [SFieldA]   -- ^ Properties of class.
    , _ciMethods   :: [FnDefA]    -- ^ Methods of class.
    , _ciHierarchy :: Maybe Ident -- ^ Potential parent.
    } deriving (Eq, Ord, Show, Read)

-- | 'ClassDAG': 'ClassGraph' augmented with 'ClassToCGNode'.
type ClassDAG      = (ClassToCGNode, ClassGraph)

-- | 'ClassDefMap': Graph of classes, edges denote inheritance.
type ClassGraph    = GF.Flip G.Gr () ClassInfo

-- | 'CGNode': Node in 'ClassGraph'.
type CGNode        = G.LNode ClassInfo

-- | 'CGEdge': Edge in 'ClassGraph'.
type CGEdge        = G.UEdge

-- | 'ClassToCGNode': Map for going from type name to 'CGNode' in 'ClassGraph'.
type ClassToCGNode = Map Ident G.Node

--------------------------------------------------------------------------------
-- Operating Environment:
--------------------------------------------------------------------------------

-- | 'TCEnv': The operating environment of an 'TCComp' computation.
data TCEnv = TCEnv {
      _reserved   :: ReservedTypes  -- ^ Map of reserved type names -> TypeA.
    , _structs    :: StructDefMap   -- ^ Map of type names -> StructDefA
    , _classGraph :: ClassDAG       -- ^ Class graph.
    , _functions  :: FnSigMap       -- ^ Map of ident -> function signatures.
    , _contexts   :: Contexts }     -- ^ Stack of contexts.
    deriving (Eq, Show, Read)

concat <$> mapM (\n -> (++) <$> makeLenses n <*> makePrisms n)
    [''ClassInfo, ''TCEnv]

-- | 'initialTCEnv': The initial empty typechecker environment.
initialTCEnv :: TCEnv
initialTCEnv = TCEnv empty empty (empty, GF.empty) empty [empty]

--------------------------------------------------------------------------------
-- Computations in compiler:
--------------------------------------------------------------------------------

-- | 'IOTCComp': A computation in typechecker gifted with IO powers, use wisely.
type IOTCComp a = IOComp TCEnv a

-- | 'TCComp': A computation in typechecker using environment 'TCEnv'.
type TCComp a = Comp TCEnv a

--------------------------------------------------------------------------------
-- Type operations:
--------------------------------------------------------------------------------

lookupMethod :: (Ident -> TCComp FnDefA) -> Ident -> ClassInfo -> TCComp FnDefA
lookupMethod onErr name cl =
    maybeErr (onErr name) (find ((name ==) . _fIdent) $ _ciMethods cl)

lookupClass :: (Ident -> TCComp G.Node) -> Ident -> TCComp ClassInfo
lookupClass onErr name = do
    (cgni, graph) <- use classGraph
    fromJust . GF.lab graph <$> maybeErr (onErr name) (lookup name cgni)

getClass :: G.Node -> TCComp ClassInfo
getClass node = uses classGraph (fromJust . flip GF.lab node . snd)

extendStruct :: (Ident -> TypeA -> TCComp ())
             -> TypeA -> Ident -> [SFieldA] -> TCComp ()
extendStruct = extendTypeX structs

lookupStruct :: (Ident -> TCComp [SFieldA]) -> Ident -> TCComp [SFieldA]
lookupStruct = lookupX structs

-- | 'lookupTypeName': If name is bound, returns the corresponding type.
-- Fails with an error otherwise.
lookupTypeName :: (Ident -> TCComp TypeA) -> Ident -> TCComp TypeA
lookupTypeName = lookupX reserved

-- | 'extendTypeX': Extends current map of type names with the one given,
-- or fails with onErr if the type name already exists.
-- Also puts a binding into the given part of the state on success.
extendTypeX :: ASetter TCEnv TCEnv (Map Ident a) (Map Ident a)
               -> (Ident -> TypeA -> TCComp ())
               -> TypeA -> Ident -> a -> TCComp ()
extendTypeX trav onErr typ name binding =
    extendTypeName onErr name typ >> trav %= insert name binding

-- | 'extendTypeName': Extends current map of type names with the one given,
-- or fails with onErr if the type name already exists.
extendTypeName :: (Ident -> TypeA -> TCComp ()) -> Ident -> TypeA -> TCComp ()
extendTypeName onErr name typ =
    reserved %>= maybe (return ()) (onErr name) . lookup name <<=>
                 return . insert name typ

--------------------------------------------------------------------------------
-- Variable operations:
--------------------------------------------------------------------------------

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

-- | 'extendVar': Extends the current scope with the given variable with name
-- and it's additional information. If variable exists, onError is used.
extendVar' :: (Ident -> TCComp ()) -> Var -> TCComp ()
extendVar' onErr var = do
    let name = vident var
    (c : ctxs) <- use contexts
    maybe (contexts .= insert name var c:ctxs)
          (const $ onErr name)
          (lookup name c)

--------------------------------------------------------------------------------
-- Function operations:
--------------------------------------------------------------------------------

-- | 'lookupFun': If function with given identifier exists, the 'FunSig' of it
-- is 'return':ed, otherwise, onErr is given the (fun = 'Ident').
lookupFun' :: (Ident -> TCComp FunSig) -> Ident -> TCComp FunSig
lookupFun' = lookupX functions

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