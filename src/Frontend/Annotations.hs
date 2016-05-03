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
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, LambdaCase,
             UndecidableInstances, FlexibleInstances, FlexibleContexts #-}

module Frontend.Annotations (
    -- * Types
    ASTAnot(..), ASTAnots,
    Kind(..), WillExecute(..), Literal(..),

    ProgramA, TopDefA, ArgA, BlockA, StmtA, ItemA,
    TypeA, ExprA, AddOpA, MulOpA, RelOpA,

    Annotated, ML,

    AnotExtract,

    -- * Operations
    toWillExecute, showKind, emptyAnot, applyEA,
    _LBool, _LInt, _LDouble, _LString,
    litBool, litDouble, litInt, litStr,
    _AWillExec, _ACExprLit,
    anotCExprLit, anotKind, anotType, anotWillExec,
    int, conststr, doub, bool, tvoid,
    
    (<@>), (+@), annotate, extractAnot,
    addTyp, addTyp', addKind, addWE, addWE', addLit, addLit'
) where

import Data.Data

import qualified Data.Generics.Uniplate.Data as U

import Control.Lens hiding (Context, contexts, Empty)
import Control.Lens.Extras (is)

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

-- | 'Literal': Annotation for the literal that constant expressions result in.
data Literal = LBool   { _litBool   :: Bool    } |
               LInt    { _litInt    :: Integer } |
               LDouble { _litDouble :: Double }  |
               LString { _litStr    :: String }
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
-- Annotations:
--------------------------------------------------------------------------------

type ML = Maybe Literal

-- | 'ASTAnot': The annotations allowed in a Javalette AST.
data ASTAnot = AType     { _anotType     :: Type [ASTAnot] } |
               AWillExec { _anotWillExec :: WillExecute    } |
               ACExprLit { _anotCExprLit :: ML             } |
               AKind     { _anotKind     :: Kind           }
    deriving (Eq, Show, Read, Data, Typeable)

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
-- Annotations, Kinds for primitive types:
--------------------------------------------------------------------------------

appConcrete :: (ASTAnots -> TypeA) -> TypeA
appConcrete typ = typ [AKind KConcrete]

conststr :: TypeA
conststr = appConcrete ConstStr

int :: TypeA
int = appConcrete Int

doub :: TypeA
doub = appConcrete Doub

bool :: TypeA
bool = appConcrete Bool

tvoid :: TypeA
tvoid = appConcrete Void

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
-- AST Annotations, Shallow mapping:
--------------------------------------------------------------------------------

-- | 'Annotated': annotatable nodes.
class Annotated f where
    -- | 'annotate': changes the annotation of a node and only that node.
    -- Unlike 'fmap', it does not map all subnodes, and the type of the
    -- annotation must be the same before and after.
    annotate :: (a -> a) -> f a -> f a

-- | '(<@>)': infix version of annotating operator.
(<@>) :: Annotated f => (a -> a) -> f a -> f a
(<@>) = annotate

-- | '(+@)': given an annotated structure, adds an annotation to it.
(+@) :: Annotated f => f [a] -> a -> f [a]
n +@ a = (++ [a]) <@> n

instance Annotated Program where annotate f (Program a td) = Program (f a) td
instance Annotated TopDef  where
    annotate f (FnDef a t i args b) = FnDef (f a) t i args b
instance Annotated Arg     where annotate f (Arg a t i) = Arg (f a) t i
instance Annotated Block   where annotate f (Block a s) = Block (f a) s
instance Annotated Stmt    where
    annotate f x = case x of
        Empty    a       -> Empty    (f a)
        BStmt    a b     -> BStmt    (f a) b
        Decl     a t i   -> Decl     (f a) t i
        Ass      a i e   -> Ass      (f a) i e
        Incr     a i     -> Incr     (f a) i
        Decr     a i     -> Decr     (f a) i
        Ret      a e     -> Ret      (f a) e
        VRet     a       -> VRet     (f a)
        Cond     a c s   -> Cond     (f a) c s
        CondElse a c i e -> CondElse (f a) c i e
        While    a c s   -> While    (f a) c s
        SExp     a e     -> SExp     (f a) e
instance Annotated Item    where
    annotate f x = case x of
        NoInit a i   -> NoInit (f a) i
        Init   a i e -> Init   (f a) i e
instance Annotated Type    where
    annotate f (Fun a r as) = Fun (f a) r as
    annotate f x            = fmap f x
instance Annotated Expr    where
    annotate f x = case x of
        EVar      a i     -> EVar      (f a) i
        ELitInt   a i     -> ELitInt   (f a) i
        ELitDoub  a d     -> ELitDoub  (f a) d
        ELitTrue  a       -> ELitTrue  (f a)
        ELitFalse a       -> ELitFalse (f a)
        EApp      a i e   -> EApp      (f a) i e
        EString   a s     -> EString   (f a) s
        Neg       a e     -> Neg       (f a) e
        Not       a e     -> Not       (f a) e
        EMul      a l o r -> EMul      (f a) l o r
        EAdd      a l o r -> EAdd      (f a) l o r
        ERel      a l o r -> ERel      (f a) l o r
        EAnd      a l   r -> EAnd      (f a) l   r
        EOr       a l   r -> EOr       (f a) l   r
instance Annotated AddOp   where annotate = fmap
instance Annotated MulOp   where annotate = fmap
instance Annotated RelOp   where annotate = fmap

-- | 'addKind': adds a kind annotation to a node.
addKind :: Annotated f => f ASTAnots -> Kind -> (f ASTAnots, Kind)
addKind x kind = (x +@ AKind kind, kind)

-- | 'addWE': adds a WillExecute annotation to a node.
addWE :: Annotated f => WillExecute -> f ASTAnots -> f ASTAnots
addWE we x = f <@> x
    where f as = if any (is _AWillExec) as then as else as ++ [AWillExec we]

-- | 'addWE'': adds a WillExecute annotation to a node.
addWE' :: (Applicative m, Annotated f)
       => f ASTAnots -> WillExecute -> t -> m (f ASTAnots, t)
addWE' stmt we hasR = pure (addWE we stmt, hasR)

-- | 'addLit': adds a maybe constant literal annotation to a node.
addLit :: Annotated f => f ASTAnots -> ML -> (f ASTAnots, ML)
addLit  expr lit = (expr +@ ACExprLit lit, lit)

-- | 'addLit'': adds a constant literal annotation to a node.
addLit' :: Annotated f => f ASTAnots -> Literal -> (f ASTAnots, ML)
addLit' expr = addLit expr . Just

-- | 'addTyp': adds a type annotation to a node.
addTyp :: Annotated f => f [ASTAnot] -> TypeA -> (f [ASTAnot], TypeA)
addTyp  expr typ = (expr +@ AType typ, typ)

-- | 'addTyp'': adds a type annotation to a node.
addTyp' :: (Monad m, Annotated f)
        => f [ASTAnot] -> TypeA -> m (f [ASTAnot], TypeA)
addTyp' expr = return . addTyp expr

--------------------------------------------------------------------------------
-- AST Annotations, Shallow getting:
--------------------------------------------------------------------------------

-- | 'AnotExtract': exposes the anotation in some annotated structure.
class AnotExtract f where
    -- | 'extractAnot': extracts the annotation.
    extractAnot :: f ASTAnots -> ASTAnots

instance Data (f ASTAnots) => AnotExtract f where
    extractAnot = head . U.childrenBi