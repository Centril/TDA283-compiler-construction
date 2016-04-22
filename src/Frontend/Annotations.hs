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
    _LBool, _LInt, _LDouble, _LString,
    litBool, litDouble, litInt, litStr,
    anotCExprLit, anotKind, anotType, anotWillExec
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

-- | 'showKind': pretty prints the 'Kind' using haskell convention.
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

-- | 'Literal': Annotation for the literal that constant expressions result in.
data Literal = LBool   { _litBool   :: Bool    }    |
               LInt    { _litInt    :: Integer } |
               LDouble { _litDouble :: Double }  |
               LString { _litStr    :: String }
    deriving (Eq, Show, Read, Ord)

makeLenses ''Literal
makePrisms ''Literal

-- | 'WillExecute': Annotation for when statements will or will not be executed.
data WillExecute = Always | Never | Unknown
    deriving (Eq, Show, Read, Ord, Enum)

-- | 'toWillExecute': Convert from 'Maybe' 'Bool' to 'WillExecute'.
toWillExecute :: Maybe Bool -> WillExecute
toWillExecute (Just True)  = Always
toWillExecute (Just False) = Never
toWillExecute Nothing      = Unknown

--------------------------------------------------------------------------------
-- Annotations:
--------------------------------------------------------------------------------

-- | 'ASTAnot': The annotations allowed in a Javalette AST.
data ASTAnot = AType     { _anotType     :: Type [ASTAnot] } |
               AWillExec { _anotWillExec :: WillExecute    } |
               ACExprLit { _anotCExprLit :: Literal        } |
               AKind     { _anotKind     :: Kind           }
    deriving (Eq, Show, Read)

makePrisms ''ASTAnot
makeLenses ''ASTAnot

-- | 'ASTAnot': Annotations added to a 'Program' AST which can be safely
-- 'void':ed away.
type ASTAnots = [ASTAnot]

-- | 'emptyAnot': no annotations, for nodes where annotations have no meaning.
emptyAnot :: ASTAnots
emptyAnot = []

-- | 'applyEA': given a node constructor that accepts an 'ASTAnots',
-- partially applies the empty annotation to it.
applyEA :: [ASTAnots -> a] -> [a]
applyEA = fmap ($ emptyAnot)

--------------------------------------------------------------------------------
-- AST Annotations, Aliases:
--------------------------------------------------------------------------------

-- | 'ProgramA': 'Program' annotated with 'ASTAnots'.
type ProgramA = Program ASTAnots

-- | 'TopDefA': 'TopDef' annotated with 'ASTAnots'.
type TopDefA  = TopDef  ASTAnots

-- | 'ArgA': 'Arg' annotated with 'ASTAnots'.
type ArgA     = Arg     ASTAnots

-- | 'BlockA': 'Block' annotated with 'ASTAnots'.
type BlockA   = Block   ASTAnots

-- | 'StmtA': 'Stmt' annotated with 'ASTAnots'.
type StmtA    = Stmt    ASTAnots

-- | 'ItemA': 'Item' annotated with 'ASTAnots'.
type ItemA    = Item    ASTAnots

-- | 'TypeA': 'Type' annotated with 'ASTAnots'.
type TypeA    = Type    ASTAnots

-- | 'ExprA': 'Expr' annotated with 'ASTAnots'.
type ExprA    = Expr    ASTAnots

-- | 'AddOpA': 'AddOp' annotated with 'ASTAnots'.
type AddOpA   = AddOp   ASTAnots

-- | 'MulOpA': 'MulOp' annotated with 'ASTAnots'.
type MulOpA   = MulOp   ASTAnots

-- | 'RelOpA': 'RelOp' annotated with 'ASTAnots'.
type RelOpA   = RelOp   ASTAnots