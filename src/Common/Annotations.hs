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
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TupleSections,
             TypeSynonymInstances, FlexibleInstances #-}

module Common.Annotations (
    -- * Types

    -- ** Annotations
    ASTAnot(..), AnotKey (..), ASTAnots,
    Kind(..), WillExecute(..), Literal(..), ML, VarSource(..), IsVirtual,

    -- ** AST Aliases
    ProgramA, TopDefA,
    ClassDefA, ClassPartA, ClassHierarchyA,
    StructDefA, SFieldA, TypeDefA,
    FnDefA, ArgA, BlockA, StmtA, ItemA,
    TypeA, DimTA, LValueA, ExprA, DimEA, AddOpA, MulOpA, RelOpA,

    -- * Operations

    -- ** General
    toWillExecute, showKind, showVS,
    emptyAnot, appConcrete, defaultVal,

    -- ** Prisms, Lenses
    _LitBool, _LitInt, _LitDouble, _LitString,
    litBool, litDouble, litInt, litStr,
    _AWillExec, _ACExprLit,
    anotCExprLit, anotKind, anotType, anotWillExec, anotVS, anotIsVirt,

    -- ** Primitive types
    int, conststr, doub, bool, tvoid,

    -- ** Arrays
    arrayT, notArray, isPointable,

    -- ** Addition
    (+@),
    addTyp, addTyp', addKind,
    addWE, addWE', always, addLit, addLit',
    addSource,

    -- ** Extraction
    extractType, extractWE, extractCELit, extractKind, extractVS,
    getType, getWE, getCELit, getKind, getVS,
    mayType, mayWE, mayCELit, mayKind, mayVS
) where

import Prelude hiding (lookup)

import Data.Data
import Data.Maybe
import Data.Monoid
import Data.Map (Map, empty, singleton, alter, lookup)

import Control.Monad

import Control.Lens hiding (Context, contexts, Empty)
import Control.Lens.Extras

import Utils.Shallow
import Utils.Sizeables

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

-- | 'IsVirtual': denotes if a method is virtual (dynamic dispatch) or not.
type IsVirtual = Bool

data AnotKey
    = AKType | AKWillExec | AKCExprLit | AKKind | AKVarSource | AKIsVirtual
    deriving (Eq, Ord, Enum, Show, Read, Data, Typeable)

-- | 'ASTAnot': The annotations allowed in a Javalette AST.
data ASTAnot = AType      { _anotType     :: Type (Map AnotKey ASTAnot) }
             | AWillExec  { _anotWillExec :: WillExecute }
             | ACExprLit  { _anotCExprLit :: ML          }
             | AKind      { _anotKind     :: Kind        }
             | AVarSource { _anotVS       :: VarSource   }
             | AIsVirtual { _anotIsVirt   :: IsVirtual   }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

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

defaultVal :: TypeA -> ExprA
defaultVal typ = fst $ flip addTyp typ $ case typ of
    Int  _     -> ELitInt   emptyAnot 0
    Doub _     -> ELitDoub  emptyAnot 0
    Bool _     -> ELitFalse emptyAnot
    Array   {} -> ECastNull emptyAnot typ
    TStruct {} -> ECastNull emptyAnot typ
    TRef    {} -> ECastNull emptyAnot typ
    x          -> error $ "defaultVal is not defined for type " ++ show x

--------------------------------------------------------------------------------
-- Arrays & related:
--------------------------------------------------------------------------------

instance Growable TypeA where
    grow x = arrayT $ maybe (x, 1) (\(_, b, c) -> (b, 1 + length c)) $
                            x ^? _Array

arrayT :: (TypeA, Int) -> TypeA
arrayT (base, dim) = appConcrete make
    where make a = Array a base $ replicate dim $ DimenT emptyAnot

instance Shrinkable TypeA where
    shrink x = maybe x sarray $ x ^? _Array
        where sarray (a, b, dst) = if length dst > 1 then Array a b $ tail dst
                                   else b

isPointable :: Type a -> Bool
isPointable x = any ($ x) [is _Array, is _TStruct]

notArray :: Type a -> Bool
notArray = isn't _Array

--------------------------------------------------------------------------------
-- AST Annotations, Aliases:
--------------------------------------------------------------------------------

-- | 'ProgramA': 'Program' annotated with 'ASTAnots'.
type ProgramA        = Program        ASTAnots

-- | 'TopDefA': 'TopDef' annotated with 'ASTAnots'.
type TopDefA         = TopDef         ASTAnots

-- | 'ClassDefA': 'ClassDef' annotated with 'ASTAnots'.
type ClassDefA       = ClassDef       ASTAnots

-- | 'ClassPartA': 'ClassPart' annotated with 'ASTAnots'.
type ClassPartA      = ClassPart      ASTAnots

-- | 'ClassHierarchyA': 'ClassHierarchy' annotated with 'ASTAnots'.
type ClassHierarchyA = ClassHierarchy ASTAnots

-- | 'StructDefA': 'StructDef' annotated with 'ASTAnots'.
type StructDefA      = StructDef      ASTAnots

-- | 'SFieldA': 'SField' annotated with 'ASTAnots'.
type SFieldA         = SField         ASTAnots

-- | 'TypeDefA': 'TypeDef' annotated with 'ASTAnots'.
type TypeDefA        = TypeDef        ASTAnots

-- | 'FnDefA': 'FnDef' annotated with 'ASTAnots'.
type FnDefA          = FnDef          ASTAnots

-- | 'ArgA': 'Arg' annotated with 'ASTAnots'.
type ArgA            = Arg            ASTAnots

-- | 'BlockA': 'Block' annotated with 'ASTAnots'.
type BlockA          = Block          ASTAnots

-- | 'StmtA': 'Stmt' annotated with 'ASTAnots'.
type StmtA           = Stmt           ASTAnots

-- | 'ItemA': 'Item' annotated with 'ASTAnots'.
type ItemA           = Item           ASTAnots

-- | 'TypeA': 'Type' annotated with 'ASTAnots'.
type TypeA           = Type           ASTAnots

-- | 'DimTA': 'DimT' annotated with 'ASTAnots'.
type DimTA           = DimT           ASTAnots

-- | 'LValueA': 'LValue' annotated with 'ASTAnots'.
type LValueA         = LValue         ASTAnots

-- | 'ExprA': 'Expr' annotated with 'ASTAnots'.
type ExprA           = Expr           ASTAnots

-- | 'AddOpA': 'AddOp' annotated with 'ASTAnots'.
type AddOpA          = AddOp          ASTAnots

-- | 'MulOpA': 'MulOp' annotated with 'ASTAnots'.
type MulOpA          = MulOp          ASTAnots

-- | 'RelOpA': 'RelOp' annotated with 'ASTAnots'.
type RelOpA          = RelOp          ASTAnots

-- | 'DimEA': 'DimE' annotated with 'ASTAnots'.
type DimEA           = DimE           ASTAnots

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

addVirt :: Shallowable f => f ASTAnots -> IsVirtual -> f ASTAnots
addVirt x v = (AKIsVirtual, AIsVirtual v) +@ x

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

--------------------------------------------------------------------------------
-- AST Annotations, Fetching:
--------------------------------------------------------------------------------

extractType :: Extractable f => f ASTAnots -> TypeA
extractType = getType . extract

extractWE :: Extractable f => f ASTAnots -> Maybe WillExecute
extractWE = mayWE . extract

extractCELit :: Extractable f => f ASTAnots -> ML
extractCELit = mayCELit . extract

extractKind :: Extractable f => f ASTAnots -> Kind
extractKind = getKind . extract

extractVS :: Extractable f => f ASTAnots -> VarSource
extractVS = getVS . extract

extractVirt :: Extractable f => f ASTAnots -> IsVirtual
extractVirt = getVirt . extract

getType :: ASTAnots -> TypeA
getType = fromJust . mayType

getWE :: ASTAnots -> WillExecute
getWE =  fromJust . mayWE

getCELit :: ASTAnots -> Literal
getCELit = fromJust . mayCELit

getKind :: ASTAnots -> Kind
getKind = fromJust . mayKind

getVS :: ASTAnots -> VarSource
getVS = fromJust . mayVS

getVirt :: ASTAnots -> IsVirtual
getVirt = fromMaybe False . mayAnot AKIsVirtual _AIsVirtual

mayType :: ASTAnots -> Maybe TypeA
mayType = mayAnot AKType _AType

mayWE :: ASTAnots -> Maybe WillExecute
mayWE = mayAnot AKWillExec _AWillExec

mayCELit :: ASTAnots -> ML
mayCELit = join . mayAnot AKCExprLit _ACExprLit

mayKind :: ASTAnots -> Maybe Kind
mayKind = mayAnot AKKind _AKind

mayVS :: ASTAnots -> Maybe VarSource
mayVS = mayAnot AKVarSource _AVarSource

mayAnot :: Ord k => k -> Getting (First a) b a -> Map k b -> Maybe a
mayAnot key _prism = lookup key >=> (^? _prism)