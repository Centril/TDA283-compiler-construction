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
    Eval, EvalResult,
    TCEnv(..), Context, Contexts, Var(..),
    FunSig(..), FunId(..), FnSigMap,

    -- * Operations
    initialTCEnv, functions, contexts, toFunId,
    lookupVar', lookupFun',
    extendVar', extendFun',
) where

import Prelude hiding (lookup)
import Data.Map (Map, empty, lookup, insert)

import Control.Monad()
import Control.Applicative()

import Control.Lens hiding (Context, contexts)

import Utils.Monad

import Common.StateOps as X
import Common.Computation as X
import Common.AST

import Frontend.Annotations as X

--------------------------------------------------------------------------------
-- Scopes / Contexts:
--------------------------------------------------------------------------------

-- | 'Context': A context for a scope, map from variables -> types.
type Context = Map Ident TypeA

-- | 'Contexts': List of 'Context'
type Contexts = [Context]

-- | 'Var': a variable specified by its 'Ident' and 'Type'.
data Var = Var { vident :: Ident, vtype :: TypeA }
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
-- Operating Environment:
--------------------------------------------------------------------------------

-- | 'TCEnv': The operating environment of an 'Eval' computation.
data TCEnv = TCEnv {
    _functions :: FnSigMap,  -- ^ Map of ident -> function signatures.
    _contexts  :: Contexts } -- ^ Stack of contexts.
    deriving (Eq, Show, Read)

makeLenses ''TCEnv

-- | 'initialTCEnv': The initial empty typechecker environment.
initialTCEnv :: TCEnv
initialTCEnv = TCEnv empty [empty]

--------------------------------------------------------------------------------
-- Computations in compiler:
--------------------------------------------------------------------------------

-- | 'Eval': A computation in typechecker using environment 'TCEnv'.
type Eval a = Comp TCEnv a

-- | 'EvalResult': result of an 'Eval' computation.
type EvalResult a = CompResult TCEnv a

--------------------------------------------------------------------------------
-- Environment operations:
--------------------------------------------------------------------------------

-- | 'lookupVar': If var exists in any scope in the 'Contexts', the 'Type' of
-- the identifier is 'return':ed, otherwise onErr is given the (var = 'Ident').
lookupVar' :: (Ident -> Eval TypeA) -> Ident -> Eval TypeA
lookupVar' onErr var = uses contexts (ctxFirst var) >>= maybeErr (onErr var)

-- | 'lookupFun': If function with given identifier exists, the 'FunSig' of it
-- is 'return':ed, otherwise, onErr is given the (fun = 'Ident').
lookupFun' :: (Ident -> Eval FunSig) -> Ident -> Eval FunSig
lookupFun' onErr fun = uses functions (lookup fun) >>= maybeErr (onErr fun)

-- | 'extendVar': Extends the current scope with the given variable with name
-- as 'Ident' and typ as 'Type'. If variable exists, onError is used.
extendVar' :: (Ident -> Eval ()) -> Var -> Eval ()
extendVar' onErr (Var name typ) = do
    (c : ctxs) <- use contexts
    maybe (contexts .= insert name typ c:ctxs)
          (const $ onErr name)
          (lookup name c)

-- | 'extendVar': Extends the accumulated function signatures with the given
-- signature as 'FnId'. If function exists, onError is used.
extendFun' :: (Ident -> Eval ()) -> FunId -> Eval ()
extendFun' onErr (FunId name sig) = do
    funs <- use functions
    maybe (functions .= insert name sig funs)
          (const $ onErr name)
          (lookup name funs)