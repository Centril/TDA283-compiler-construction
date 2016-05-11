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
Module      : Common.Annotations
Description : AST Annotations in Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

AST Annotations in Javalette compiler.
-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, LambdaCase, TupleSections #-}

module Common.Annotations (
    -- * Types
    ASTAnot(..), AnotKey (..), ASTAnots,
    Kind(..), WillExecute(..), Literal(..), ML, VarSource(..),

    ProgramA, TopDefA, ArgA, BlockA, StmtA, ItemA,
    TypeA, ExprA, AddOpA, MulOpA, RelOpA,

    -- * Operations
    toWillExecute, showKind, emptyAnot, showVS,
    _LitBool, _LitInt, _LitDouble, _LitString,
    litBool, litDouble, litInt, litStr,
    _AWillExec, _ACExprLit,
    anotCExprLit, anotKind, anotType, anotWillExec, anotVS,
    int, conststr, doub, bool, tvoid, defaultVal,

    (+@),
    addTyp, addTyp', addKind,
    addWE, addWE', always, addLit, addLit',
    addSource
) where

import Data.Data

import Data.Map (Map, empty, singleton, alter)

import Control.Lens hiding (Context, contexts, Empty)

import Utils.Shallow

import Common.AST

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
    deriving (Eq, Show, Read, Ord, Data, Typeable)

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

type ML = Maybe Literal

-- | 'Literal': Annotation for the literal that constant expressions result in.
data Literal = LitBool   { _litBool   :: Bool    } |
               LitInt    { _litInt    :: Integer } |
               LitDouble { _litDouble :: Double }  |
               LitString { _litStr    :: String }
    deriving (Eq, Show, Read, Ord, Data, Typeable)

makeLenses ''Literal
makePrisms ''Literal

-- | 'WillExecute': Annotation for when statements will or will not be executed.
data WillExecute = Always | Never | Unknown
    deriving (Eq, Show, Read, Ord, Enum, Data, Typeable)

-- | 'toWillExecute': Convert from 'Maybe' 'Bool' to 'WillExecute'.
toWillExecute :: Maybe Bool -> WillExecute
toWillExecute (Just True)  = Always
toWillExecute (Just False) = Never
toWillExecute Nothing      = Unknown

--------------------------------------------------------------------------------
-- Variable reference sources:
--------------------------------------------------------------------------------

-- | 'VarSource': Annotation for a source of an 'EVar'.
data VarSource = VSArg | VSLocal
    deriving (Eq, Ord, Enum, Show, Read, Data, Typeable)

makePrisms ''VarSource

showVS :: VarSource -> String
showVS VSArg   = "argument"
showVS VSLocal = "variable"

--------------------------------------------------------------------------------
-- Annotations:
--------------------------------------------------------------------------------

data AnotKey = AKType | AKWillExec | AKCExprLit | AKKind | AKVarSource
    deriving (Eq, Ord, Enum, Show, Read, Data, Typeable)

-- | 'ASTAnot': The annotations allowed in a Javalette AST.
data ASTAnot = AType      { _anotType     :: Type (Map AnotKey ASTAnot) }
             | AWillExec  { _anotWillExec :: WillExecute }
             | ACExprLit  { _anotCExprLit :: ML          }
             | AKind      { _anotKind     :: Kind        }
             | AVarSource { _anotVS       :: VarSource   }
    deriving (Eq, Show, Read, Data, Typeable)

makePrisms ''ASTAnot
makeLenses ''ASTAnot

-- | 'ASTAnot': Annotations added to a 'Program' AST which can be safely
-- 'void':ed away.
type ASTAnots = Map AnotKey ASTAnot

-- | 'emptyAnot': no annotations, for nodes where annotations have no meaning.
emptyAnot :: ASTAnots
emptyAnot = empty

addToEmpty :: AnotKey -> ASTAnot -> (ASTAnots -> a) -> a
addToEmpty k a ctor = ctor $ singleton k a

--------------------------------------------------------------------------------
-- Annotations, Kinds for primitive types:
--------------------------------------------------------------------------------

appConcrete :: (ASTAnots -> TypeA) -> TypeA
appConcrete = addToEmpty AKKind $ AKind KConcrete

conststr, int, doub, bool, tvoid :: TypeA
conststr = appConcrete ConstStr
int      = appConcrete Int
doub     = appConcrete Doub
bool     = appConcrete Bool
tvoid    = appConcrete Void

-- TODO: Fix the annotations and move maybe
defaultVal :: TypeA -> ExprA
defaultVal typ = case typ of
    Int  _ -> ELitInt   emptyAnot 0
    Doub _ -> ELitDoub  emptyAnot 0
    Bool _ -> ELitFalse emptyAnot
    x      -> error $ "default value not defined for type " ++ show x

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

--------------------------------------------------------------------------------
-- AST Annotations, helpers:
--------------------------------------------------------------------------------

-- | '(+@)': given an annotation map and a structure with that, adds an
-- annotation to it. If an annotation of that type already exists, nothing
-- happens. In other words, from the perspective of this function, the annotated
-- annotation map is "insert once only".
(+@) :: Shallowable f => (AnotKey, ASTAnot) -> f ASTAnots -> f ASTAnots
(k, a) +@ n = alter (maybe (Just a) Just) k <@> n

addH :: Shallowable f
     => (t -> (AnotKey, ASTAnot)) -> f ASTAnots -> t -> (f ASTAnots, t)
addH f x a = (f a +@ x, a)

-- | 'addKind': adds a kind annotation to a node.
addKind :: Shallowable f => f ASTAnots -> Kind -> (f ASTAnots, Kind)
addKind = addH $ (AKKind,) . AKind

-- | 'addWE': adds a WillExecute annotation to a node.
addWE :: Shallowable f => WillExecute -> f ASTAnots -> f ASTAnots
addWE we x = (AKWillExec, AWillExec we) +@ x

-- | 'addWE'': adds a WillExecute annotation to a node.
addWE' :: (Applicative m, Shallowable f)
       => f ASTAnots -> WillExecute -> t -> m (f ASTAnots, t)
addWE' stmt we = pure . (addWE we stmt,)

-- | 'always': adds an Always annotation to a node.
always :: Shallowable f => f ASTAnots -> f ASTAnots
always = addWE Always

-- | 'addLit': adds a maybe constant literal annotation to a node.
addLit :: Shallowable f => f ASTAnots -> ML -> (f ASTAnots, ML)
addLit = addH $ (AKCExprLit,) . ACExprLit

-- | 'addLit'': adds a constant literal annotation to a node.
addLit' :: Shallowable f => f ASTAnots -> Literal -> (f ASTAnots, ML)
addLit' x = addLit x . Just

-- | 'addTyp': adds a type annotation to a node.
addTyp :: Shallowable f => f ASTAnots -> TypeA -> (f ASTAnots, TypeA)
addTyp = addH $ (AKType,) . AType

-- | 'addTyp'': adds a type annotation to a node.
addTyp' :: (Applicative m, Shallowable f)
        => f ASTAnots -> TypeA -> m (f ASTAnots, TypeA)
addTyp' x = pure . addTyp x

addSource :: Shallowable f => f ASTAnots -> VarSource -> f ASTAnots
addSource x s = (AKVarSource, AVarSource s) +@ x