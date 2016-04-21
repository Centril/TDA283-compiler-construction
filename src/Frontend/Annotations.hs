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
Module      : Frontend.Annotations
Description : AST Annotations in Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

AST Annotations in Javalette compiler.
-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Annotations (
    -- * Types
    ASTAnot(..), ASTAnots,
    Kind(..), WillExecute(..), Literal(..),

    ProgramA, TopDefA, ArgA, BlockA, StmtA, ItemA,
    TypeA, ExprA, AddOpA, MulOpA, RelOpA,

    -- * Operations
    toWillExecute, showKind, emptyAnot, applyEA,
    _LBool, _LInt, _LDouble, _LString
) where

import Control.Lens hiding (Context, contexts)

import Javalette.Abs

--------------------------------------------------------------------------------
-- Kinds:
--------------------------------------------------------------------------------

-- | 'Kind': Models kinds of 'Type' in the Javalette language.
-- 'KConcrete' denotes the concrete kind,
-- 'KArrow' denotes a type constructor from one kind to another.
-- 'KConstraint' is special and used for constraints on types.
--
-- Note: for now, all types live in 'KConcrete'.
data Kind = KConcrete | KConstraint |
            KArrow { kaFrom :: Kind, kaTo :: Kind }
    deriving (Eq, Show, Read, Ord)

showKind :: Kind -> String
showKind KConcrete    = "*"
showKind KConstraint  = "Constraint"
showKind (KArrow f t) = unwords [showf, "->", show t]
    where showf   = case f of
                    KArrow _ _ -> concat ["(", show f, ")"]
                    _          -> show f

--------------------------------------------------------------------------------
-- Literal & WillExecute:
--------------------------------------------------------------------------------

data Literal = LBool Bool | LInt Integer | LDouble Double | LString String
    deriving (Eq, Show, Read, Ord)

makePrisms ''Literal

data WillExecute = Always | Never | Unknown
    deriving (Eq, Show, Read, Ord, Enum)

toWillExecute :: Maybe Bool -> WillExecute
toWillExecute (Just True)  = Always
toWillExecute (Just False) = Never
toWillExecute Nothing      = Unknown

--------------------------------------------------------------------------------
-- Annotations:
--------------------------------------------------------------------------------

data ASTAnot = AType     { _anotType     :: Type ASTAnot } |
               AWillExec { _anotWillExec :: WillExecute  } |
               ACExprLit { _anotCExprLit :: Literal      } |
               AKind     { _anotKind     :: Kind         }
    deriving (Eq, Show, Read)

-- | 'ASTAnot': Annotations added to a 'Program' AST which can be safely
-- 'void':ed away.
type ASTAnots = [ASTAnot]

emptyAnot :: ASTAnots
emptyAnot = []

applyEA :: [ASTAnots -> a] -> [a]
applyEA = fmap ($ emptyAnot)

--------------------------------------------------------------------------------
-- AST Annotations, Aliases:
--------------------------------------------------------------------------------

type ProgramA = Program ASTAnots
type TopDefA  = TopDef  ASTAnots
type ArgA     = Arg     ASTAnots
type BlockA   = Block   ASTAnots
type StmtA    = Stmt    ASTAnots
type ItemA    = Item    ASTAnots
type TypeA    = Type    ASTAnots
type ExprA    = Expr    ASTAnots
type AddOpA   = AddOp   ASTAnots
type MulOpA   = MulOp   ASTAnots
type RelOpA   = RelOp   ASTAnots