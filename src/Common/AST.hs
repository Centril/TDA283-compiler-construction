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
Module      : Common.AST
Description : Frontend AST in Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Frontend AST in Javalette compiler.
-}
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, LambdaCase #-}

module Common.AST where

import Data.Data

import Control.Lens hiding (Empty, op)

import Utils.Shallow
import Utils.Monad

import qualified Javalette.Abs as J

--------------------------------------------------------------------------------
-- Data types:
--------------------------------------------------------------------------------

newtype Ident = Ident { _ident :: String }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data Program a = Program { _pAnot :: a, _pTopDefs :: [TopDef a] }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data TopDef a = FnDef { _fAnot :: a, _fRetTyp :: Type a, _fIdent :: Ident,
                        _fArgs  :: [Arg a], _fBlock :: Block a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data Arg a = Arg { _aAnot :: a, _aTyp :: Type a, _aIdent :: Ident }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data Block a = Block { _bAnot :: a, _bStmts :: [Stmt a] }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data Stmt a
    = Empty    { _sAnot :: a }
    | BStmt    { _sAnot :: a, _sBlock :: Block a }
    | Decl     { _sAnot :: a, _sDTyp  :: Type a, _sDItems :: [Item a] }
    | Ass      { _sAnot :: a, _sIdent :: Ident,  _sExpr   :: Expr a }
    | AssArr   { _sAnot :: a, _sIdent :: Ident,  _sDimEs  :: [DimE a],
                 _sExpr :: Expr a }
    | Incr     { _sAnot :: a, _sIdent :: Ident }
    | Decr     { _sAnot :: a, _sIdent :: Ident }
    | Ret      { _sAnot :: a, _sExpr  :: Expr a }
    | VRet     { _sAnot :: a }
    | Cond     { _sAnot :: a, _sExpr :: Expr a, _sSi :: Stmt a }
    | CondElse { _sAnot :: a, _sExpr :: Expr a, _sSi :: Stmt a, _sSe :: Stmt a }
    | While    { _sAnot :: a, _sExpr :: Expr a, _sSi :: Stmt a }
    | For      { _sAnot :: a, _sTyp  :: Type a, _sIdent :: Ident,
                 _sExpr :: Expr a, _sSi :: Stmt a }
    | SExp     { _sAnot :: a, _sExpr :: Expr a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data Item a = NoInit { _iAnot :: a, _iIdent :: Ident } |
              Init   { _iAnot :: a, _iIdent :: Ident, _iExpr :: Expr a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data Type a
    = Int      { _tAnot :: a }
    | Doub     { _tAnot :: a }
    | Bool     { _tAnot :: a }
    | Void     { _tAnot :: a }
    | Array    { _tAnot :: a, _tTyp :: Type a, _tDimTs :: [DimT a] }
    | ConstStr { _tAnot :: a }
    | Fun      { _tAnot :: a, _tfRetTyp :: Type a, _tfArgsTyps :: [Type a] }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data DimT a = DimenT { _dtAnot :: a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data Expr a
    = ENew      { _eAnot :: a, _eTyp   :: Type a, _eDimEs :: [DimE a] }
    | EVar      { _eAnot :: a, _eIdent :: Ident,  _eDimEs :: [DimE a] }
    | Length    { _eAnot :: a, _eExpr  :: Expr a }
    | ELitInt   { _eAnot :: a, _eLIVal :: Integer }
    | ELitDoub  { _eAnot :: a, _eLDVal :: Double  }
    | EString   { _eAnot :: a, _eLSVal :: String  }
    | ELitTrue  { _eAnot :: a }
    | ELitFalse { _eAnot :: a }
    | EApp      { _eAnot :: a, _eIdent :: Ident, _eAppExprs :: [Expr a] }
    | Neg       { _eAnot :: a, _eExpr  :: Expr a  }
    | Not       { _eAnot :: a, _eExpr  :: Expr a  }
    | EMul      { _eAnot :: a, _eLExpr :: Expr a, _eMOp :: MulOp a,
                               _eRExpr :: Expr a  }
    | EAdd      { _eAnot :: a, _eLExpr :: Expr a, _eAOp :: AddOp a,
                               _eRExpr :: Expr a  }
    | ERel      { _eAnot :: a, _eLExpr :: Expr a, _eROp :: RelOp a,
                               _eRExpr :: Expr a  }
    | EAnd      { _eAnot :: a, _eLExpr :: Expr a, _eRExpr :: Expr a  }
    | EOr       { _eAnot :: a, _eLExpr :: Expr a, _eRExpr :: Expr a  }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data DimE a = DimenE { _deAnot :: a, _deExpr :: Expr a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data AddOp a = Plus { _addAnot :: a } | Minus { _addAnot :: a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data MulOp a = Times { _mAnot :: a } | Div { _mAnot :: a } | Mod { _mAnot :: a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data RelOp a = LTH { _rAnot :: a } | LE  { _rAnot :: a } | GTH { _rAnot :: a } |
                GE { _rAnot :: a } | EQU { _rAnot :: a } | NE  { _rAnot :: a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

--------------------------------------------------------------------------------
-- Prisms and Lenses:
--------------------------------------------------------------------------------

makeLenses ''Ident
makeLenses ''Program
makeLenses ''TopDef
makeLenses ''Arg
makeLenses ''Block
makeLenses ''Stmt
makeLenses ''Item
makeLenses ''Type
makeLenses ''DimT
makeLenses ''Expr
makeLenses ''DimE
makeLenses ''AddOp
makeLenses ''MulOp
makeLenses ''RelOp

makePrisms ''Ident
makePrisms ''Program
makePrisms ''TopDef
makePrisms ''Arg
makePrisms ''Block
makePrisms ''Stmt
makePrisms ''Item
makePrisms ''Type
makePrisms ''DimT
makePrisms ''Expr
makePrisms ''DimE
makePrisms ''AddOp
makePrisms ''MulOp
makePrisms ''RelOp

--------------------------------------------------------------------------------
-- Functor instances:
--------------------------------------------------------------------------------

instance Functor Program where
    fmap f (Program a tds)    = Program (f a) $ f <$$> tds

instance Functor TopDef where
    fmap f (FnDef a r i ps b) = FnDef   (f a) (f <$> r) i (f <$$> ps) (f <$> b)

instance Functor Arg where
    fmap f (Arg a t i)        = Arg     (f a) (f <$> t) i

instance Functor Block where
    fmap f (Block a ss)       = Block   (f a) $ f <$$> ss

instance Functor Stmt where
    fmap f = \case
        Empty    a         -> Empty    (f a)
        BStmt    a b       -> BStmt    (f a) (f <$> b)
        Decl     a t is    -> Decl     (f a) (f <$> t) (f <$$> is)
        Ass      a i e     -> Ass      (f a) i (f <$> e)
        AssArr   a i ds e  -> AssArr   (f a) i (f <$$> ds) (f <$> e)
        Incr     a i       -> Incr     (f a) i
        Decr     a i       -> Decr     (f a) i
        Ret      a e       -> Ret      (f a) (f <$> e)
        VRet     a         -> VRet     (f a)
        Cond     a c i     -> Cond     (f a) (f <$> c) (f <$> i)
        CondElse a c i e   -> CondElse (f a) (f <$> c) (f <$> i) (f <$> e)
        While    a c i     -> While    (f a) (f <$> c) (f <$> i)
        For      a t i e s -> For      (f a) (f <$> t) i (f <$> e) (f <$> s)
        SExp     a e       -> SExp     (f a) (f <$> e)

instance Functor Item where
    fmap f = \case NoInit a i   -> NoInit (f a) i
                   Init   a i e -> Init   (f a) i (f <$> e)

instance Functor Type where
    fmap f = \case Fun      a r ps -> Fun      (f a) (f <$> r) (f <$$> ps)
                   Int      a      -> Int      (f a)
                   Doub     a      -> Doub     (f a)
                   Bool     a      -> Bool     (f a)
                   Void     a      -> Void     (f a)
                   Array    a t ds -> Array    (f a) (f <$> t) (f <$$> ds)
                   ConstStr a      -> ConstStr (f a)

instance Functor DimT where fmap = over dtAnot

instance Functor Expr where
    fmap f = \case
        ENew      a t des  -> ENew      (f a) (f <$> t) (f <$$> des)
        EVar      a i des  -> EVar      (f a) i         (f <$$> des)
        Length    a e      -> Length    (f a) (f <$> e)
        ELitInt   a v      -> ELitInt   (f a) v
        ELitDoub  a v      -> ELitDoub  (f a) v
        ELitTrue  a        -> ELitTrue  (f a)
        ELitFalse a        -> ELitFalse (f a)
        EString   a v      -> EString   (f a) v
        EApp      a i es   -> EApp      (f a) i (f <$$> es)
        Neg       a e      -> Neg       (f a) (f <$> e)
        Not       a e      -> Not       (f a) (f <$> e)
        EMul      a l op r -> EMul      (f a) (f <$> l) (f <$> op) (f <$> r)
        EAdd      a l op r -> EAdd      (f a) (f <$> l) (f <$> op) (f <$> r)
        ERel      a l op r -> ERel      (f a) (f <$> l) (f <$> op) (f <$> r)
        EAnd      a l    r -> EAnd      (f a) (f <$> l)            (f <$> r)
        EOr       a l    r -> EOr       (f a) (f <$> l)            (f <$> r)

instance Functor DimE where
    fmap f (DimenE a e) = DimenE (f a) (f <$> e)

instance Functor AddOp where fmap = over addAnot
instance Functor MulOp where fmap = over mAnot
instance Functor RelOp where fmap = over rAnot

--------------------------------------------------------------------------------
-- Overable instances:
--------------------------------------------------------------------------------

instance Overable Program where overF = pAnot
instance Overable TopDef  where overF = fAnot
instance Overable Arg     where overF = aAnot
instance Overable Block   where overF = bAnot
instance Overable Stmt    where overF = sAnot
instance Overable Item    where overF = iAnot
instance Overable Type    where overF = tAnot
instance Overable DimT    where overF = dtAnot
instance Overable Expr    where overF = eAnot
instance Overable DimE    where overF = deAnot
instance Overable AddOp   where overF = addAnot
instance Overable MulOp   where overF = mAnot
instance Overable RelOp   where overF = rAnot

--------------------------------------------------------------------------------
-- Converting:
--------------------------------------------------------------------------------

convert :: J.Program a -> Program a
convert (J.Program anot fns)          = Program anot $ cf <$> fns
    where cf  (J.FnDef a r i p b)     = FnDef     a (ct r) (ci i)
                                                    (ca <$> p) (cb b)
          ca  (J.Arg       a t i)     = Arg       a (ct t) (ci i)
          ct  (J.Int       a)         = Int       a
          ct  (J.Doub      a)         = Doub      a
          ct  (J.Bool      a)         = Bool      a
          ct  (J.Void      a)         = Void      a
          ct  (J.Array     a t ds)    = Array     a (ct t) (cdt <$> ds)
          ct  (J.ConstStr  a)         = ConstStr  a
          ct  (J.Fun       a r as)    = Fun       a (ct r) (ct <$> as)
          ci  (J.Ident     s)         = Ident     s
          cit (J.NoInit    a i)       = NoInit    a (ci i)
          cit (J.Init      a i e)     = Init      a (ci i) (ce e)
          cao (J.Plus      a)         = Plus      a
          cao (J.Minus     a)         = Minus     a
          cmo (J.Times     a)         = Times     a
          cmo (J.Div       a)         = Div       a
          cmo (J.Mod       a)         = Mod       a
          cro (J.LTH       a)         = LTH       a
          cro (J.LE        a)         = LE        a
          cro (J.GTH       a)         = GTH       a
          cro (J.GE        a)         = GE        a
          cro (J.EQU       a)         = EQU       a
          cro (J.NE        a)         = NE        a
          cb  (J.Block     a ss)      = Block     a (cs <$> ss)
          cs  (J.Empty     a)         = Empty     a
          cs  (J.BStmt     a b)       = BStmt     a (cb b)
          cs  (J.Decl      a t is)    = Decl      a (ct t) (cit <$> is)
          cs  (J.Ass       a i e)     = Ass       a (ci i) (ce e)
          cs  (J.AssArr    a i ds e)  = AssArr    a (ci i) (cde <$> ds) (ce e)
          cs  (J.Incr      a i)       = Incr      a (ci i)
          cs  (J.Decr      a i)       = Decr      a (ci i)
          cs  (J.Ret       a e)       = Ret       a (ce e)
          cs  (J.VRet      a)         = VRet      a
          cs  (J.Cond      a c i)     = Cond      a (ce c) (cs i)
          cs  (J.CondElse  a c i e)   = CondElse  a (ce c) (cs i) (cs e)
          cs  (J.While     a c i)     = While     a (ce c) (cs i)
          cs  (J.For       a t i e s) = For       a (ct t) (ci i) (ce e) (cs s)
          cs  (J.SExp      a e)       = SExp      a (ce e)
          ce  (J.ENew      a t des)   = ENew      a (ct t) (cde <$> des)
          ce  (J.EVar      a i des)   = EVar      a (ci i) (cde <$> des)
          ce  (J.Length    a e)       = Length    a (ce e)
          ce  (J.ELitInt   a v)       = ELitInt   a v
          ce  (J.ELitDoub  a v)       = ELitDoub  a v
          ce  (J.EString   a v)       = EString   a v
          ce  (J.ELitTrue  a)         = ELitTrue  a
          ce  (J.ELitFalse a)         = ELitFalse a
          ce  (J.EApp      a i es)    = EApp      a (ci i) (ce <$> es)
          ce  (J.Neg       a e)       = Neg       a (ce e)
          ce  (J.Not       a e)       = Not       a (ce e)
          ce  (J.EMul      a l o r)   = EMul      a (ce l) (cmo o) (ce r)
          ce  (J.EAdd      a l o r)   = EAdd      a (ce l) (cao o) (ce r)
          ce  (J.ERel      a l o r)   = ERel      a (ce l) (cro o) (ce r)
          ce  (J.EAnd      a l   r)   = EAnd      a (ce l)         (ce r)
          ce  (J.EOr       a l   r)   = EOr       a (ce l)         (ce r)
          cdt (J.DimenT    a)         = DimenT    a
          cde (J.DimenE    a e)       = DimenE    a (ce e)