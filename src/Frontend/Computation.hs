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
Module      : Frontend.Computation
Description : Computation monad and operations in Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Computation monad and operations in Javalette compiler.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frontend.Computation (
    -- * Types
    Eval, EvalResult,

    -- * Operations
    pushBlock, popBlock,
    lookupVar', lookupFun',
    extendVar', extendFun',
) where

import Safe

import Data.Maybe

import qualified Data.Map as Map

import Control.Monad()
import Control.Applicative()

import Control.Lens hiding (Context, contexts)

import Utils.Foldable
import Utils.Monad

import Javalette.Abs

import Common.Computation

import Frontend.Annotations
import Frontend.Environment

--------------------------------------------------------------------------------
-- Environment operations:
--------------------------------------------------------------------------------

-- | 'pushBlock': pushes a fresh and empty block to the 'Context' stack.
pushBlock :: Eval ()
pushBlock = contexts %= (Map.empty:)

-- | 'popBlock': pops the top block from the 'Context' stack.
popBlock :: Eval ()
popBlock = contexts %= (fromMaybe [] . tailMay)

-- | 'lookupVar': If var exists in any scope in the 'Contexts', the 'Type' of
-- the identifier is '  return':ed, otherwise onErr is given the (var = 'Ident').
lookupVar' :: (Ident -> Eval TypeA) -> Ident -> Eval TypeA
lookupVar' onErr var = uses contexts (_lookupVar var) >>= maybeErr (onErr var)

_lookupVar :: Ident -> Contexts -> Maybe TypeA
_lookupVar = mfind . Map.lookup

-- | 'lookupFun': If function with given identifier exists, the 'FunSig' of it
-- is 'return':ed, otherwise, onErr is given the (var = 'Ident').
lookupFun' :: (Ident -> Eval FunSig) -> Ident -> Eval FunSig
lookupFun' onErr var = uses functions (Map.lookup var) >>= maybeErr (onErr var)

-- | 'extendVar': Extends the current scope with the given variable with ident
-- as 'Ident' and typ as 'Type'. If variable exists, onError is used.
extendVar' :: (Ident -> Eval ()) -> Var -> Eval ()
extendVar' onErr (Var ident typ) = do
    (c : ctxs) <- use contexts
    maybe (contexts .= Map.insert ident typ c:ctxs)
          (const $ onErr ident)
          (Map.lookup ident c)

-- | 'extendVar': Extends the accumulated function signatures with the given
-- signature as 'FnId'. If function exists, onError is used.
extendFun' :: (Ident -> Eval ()) -> FunId -> Eval ()
extendFun' onErr (FunId ident sig) = do
    funs <- use functions
    maybe (functions .= Map.insert ident sig funs)
          (const $ onErr ident)
          (Map.lookup ident funs)

--------------------------------------------------------------------------------
-- Computations in compiler:
--------------------------------------------------------------------------------

-- | 'Eval': A computation given typechecker state environment 'TCEnv',
-- potential fail-fast errors of type 'ErrMsg' and accumulated 'InfoLog's.
-- Monadic stack: StateT -> ExceptT -> WriterT -> Identity
-- See 'EvalResult' for details.
type Eval a = Comp TCEnv a

-- | 'EvalResult': result of an 'Eval' computation.
type EvalResult a = CompResult TCEnv a