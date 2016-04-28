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
module Frontend.Computation (
    -- * Modules
    module Common.StateOps,
    module Common.Computation,
    module Frontend.Environment,

    -- * Types
    Eval, EvalResult,

    -- * Operations
    lookupVar', lookupFun',
    extendVar', extendFun',
) where

import Prelude hiding (lookup)
import Data.Map (lookup, insert)

import Control.Monad()
import Control.Applicative()

import Control.Lens hiding (Context, contexts)

import Utils.Monad

import Common.Computation
import Common.StateOps

import Frontend.Environment

import Javalette.Abs

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

-- | 'extendVar': Extends the current scope with the given variable with ident
-- as 'Ident' and typ as 'Type'. If variable exists, onError is used.
extendVar' :: (Ident -> Eval ()) -> Var -> Eval ()
extendVar' onErr (Var ident typ) = do
    (c : ctxs) <- use contexts
    maybe (contexts .= insert ident typ c:ctxs)
          (const $ onErr ident)
          (lookup ident c)

-- | 'extendVar': Extends the accumulated function signatures with the given
-- signature as 'FnId'. If function exists, onError is used.
extendFun' :: (Ident -> Eval ()) -> FunId -> Eval ()
extendFun' onErr (FunId ident sig) = do
    funs <- use functions
    maybe (functions .= insert ident sig funs)
          (const $ onErr ident)
          (lookup ident funs)

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