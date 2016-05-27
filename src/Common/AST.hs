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

data TopDef a
    = TClassDef  { _toAnot :: a, _toClassDef  :: ClassDef  a }
    | TStructDef { _toAnot :: a, _toStructDef :: StructDef a }
    | TTypeDef   { _toAnot :: a, _toTypeDef   :: TypeDef   a }
    | TFnDef     { _toAnot :: a, _toFnDef     :: FnDef     a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data ClassDef a
    = ClassDef { _cdAnot :: a, _cdIdent :: Ident,
                 _cdHierarchy :: ClassHierarchy a, _cdPart :: [ClassPart a] }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data ClassPart a
    = MethodDef { _cpAnot :: a, _cpFnDef :: FnDef a }
    | ClassProp { _cpAnot :: a, _cpType  :: Type a, _cpIdent :: Ident }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data ClassHierarchy a
    = HExtend { _chAnot :: a, _chIdent :: Ident }
    | HBase   { _chAnot :: a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data StructDef a
    = StructDef { _stAnot :: a, _stIdent :: Ident, _stFields :: [SField a] }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data TypeDef a = TypeDef { _tdAnot :: a, _tdType :: Type a, _tdIdent :: Ident }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data SField a = SField { _sfAnot :: a, _sfType :: Type a, _sfIdent :: Ident,
                         _sfIndex :: Integer }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data FnDef a = FnDef { _fAnot :: a, _fRetTyp :: Type a, _fIdent :: Ident,
                       _fArgs :: [Arg a], _fBlock :: Block a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data Arg a = Arg { _aAnot :: a, _aTyp :: Type a, _aIdent :: Ident }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data Block a = Block { _bAnot :: a, _bStmts :: [Stmt a] }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data Stmt a
    = Empty    { _sAnot :: a }
    | BStmt    { _sAnot :: a, _sBlock :: Block a }
    | Decl     { _sAnot :: a, _sDTyp  :: Type a, _sDItems :: [Item a] }
    | Assign   { _sAnot :: a, _sLVal  :: LValue a, _sExpr :: Expr a }
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
    | TStruct  { _tAnot :: a, _tIdent :: Ident }
    | TRef     { _tAnot :: a, _tIdent :: Ident }
    | ConstStr { _tAnot :: a }
    | Fun      { _tAnot :: a, _tfRetTyp :: Type a, _tfArgsTyps :: [Type a] }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data DimT a = DimenT { _dtAnot :: a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data LValue a
    = LValueV { _lvAnot  :: a, _lvIdent :: Ident,    _lvDimEs :: [DimE a] }
    | LValueS { _lvAnot  :: a, _lvLLVal :: LValue a, _lvRLVal :: LValue a }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data Expr a
    = ENew      { _eAnot :: a, _eTyp   :: Type a, _eDimEs :: [DimE a] }
    | EVar      { _eAnot :: a, _eLVal  :: LValue a }
    | ELitInt   { _eAnot :: a, _eLIVal :: Integer }
    | ELitDoub  { _eAnot :: a, _eLDVal :: Double  }
    | EString   { _eAnot :: a, _eLSVal :: String  }
    | ELitTrue  { _eAnot :: a }
    | ELitFalse { _eAnot :: a }
    | ECastNull { _eAnot :: a, _eTyp   :: Type a }
    | EApp      { _eAnot :: a, _eIdent :: Ident, _eAppExprs :: [Expr a] }
    | EMApp     { _eAnot :: a, _eExpr  :: Expr a,
                 _eIdent :: Ident, _eAppExprs :: [Expr a] }
    | Incr      { _eAnot :: a, _eLVal  :: LValue a }
    | Decr      { _eAnot :: a, _eLVal  :: LValue a }
    | PreIncr   { _eAnot :: a, _eLVal  :: LValue a }
    | PreDecr   { _eAnot :: a, _eLVal  :: LValue a }
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

concat <$> mapM (\n -> (++) <$> makeLenses n <*> makePrisms n)
    [''Ident, ''Program, ''TopDef, ''ClassDef, ''ClassPart, ''ClassHierarchy
    ,''StructDef, ''SField, ''TypeDef, ''FnDef, ''Arg, ''Block, ''Stmt, ''Item
    ,''Type, ''DimT, ''Expr, ''LValue, ''DimE, ''AddOp, ''MulOp, ''RelOp]

--------------------------------------------------------------------------------
-- Functor instances:
--------------------------------------------------------------------------------

instance Functor Program where
    fmap f (Program a tds)    = Program (f a) $ f <$$> tds

instance Functor TopDef where
    fmap f = \case
        TClassDef  a x -> TClassDef  (f a) (f <$> x)
        TStructDef a x -> TStructDef (f a) (f <$> x)
        TTypeDef   a x -> TTypeDef   (f a) (f <$> x)
        TFnDef     a x -> TFnDef     (f a) (f <$> x)

instance Functor ClassDef where
    fmap f (ClassDef a i ch cps) = ClassDef (f a) i (f <$> ch) (f <$$> cps)

instance Functor ClassPart where
    fmap f = \case
        MethodDef a fd  -> MethodDef (f a) (f <$> fd)
        ClassProp a t i -> ClassProp (f a) (f <$> t) i

instance Functor ClassHierarchy where fmap = over chAnot

instance Functor StructDef where
    fmap f (StructDef a i fs) = StructDef (f a) i (f <$$> fs)

instance Functor SField where
    fmap f (SField a t n i) = SField (f a) (f <$> t) n i

instance Functor TypeDef where
    fmap f (TypeDef a t i) = TypeDef (f a) (f <$> t) i

instance Functor FnDef where
    fmap f (FnDef a r i ps b) = FnDef (f a) (f <$> r) i (f <$$> ps) (f <$> b)

instance Functor Arg where
    fmap f (Arg a t i)        = Arg     (f a) (f <$> t) i

instance Functor Block where
    fmap f (Block a ss)       = Block   (f a) $ f <$$> ss

instance Functor Stmt where
    fmap f = \case
        Empty    a         -> Empty    (f a)
        BStmt    a b       -> BStmt    (f a) (f <$> b)
        Decl     a t is    -> Decl     (f a) (f <$> t) (f <$$> is)
        Assign   a lv e    -> Assign   (f a) (f <$> lv) (f <$> e)
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
                   TStruct  a i    -> TStruct  (f a) i
                   TRef     a i    -> TRef     (f a) i
                   ConstStr a      -> ConstStr (f a)

instance Functor DimT where fmap = over dtAnot

instance Functor LValue where
    fmap f = \case
        LValueV a i d -> LValueV (f a) i (f <$$> d)
        LValueS a l r -> LValueS (f a) (f <$> l) (f <$> r)

instance Functor Expr where
    fmap f = \case
        ENew      a t des  -> ENew      (f a) (f <$> t) (f <$$> des)
        EVar      a lv     -> EVar      (f a) (f <$> lv)
        ELitInt   a v      -> ELitInt   (f a) v
        ELitDoub  a v      -> ELitDoub  (f a) v
        ELitTrue  a        -> ELitTrue  (f a)
        ELitFalse a        -> ELitFalse (f a)
        ECastNull a t      -> ECastNull (f a) (f <$> t)
        EString   a v      -> EString   (f a) v
        EApp      a i es   -> EApp      (f a) i (f <$$> es)
        EMApp     a e i es -> EMApp     (f a) (f <$> e) i (f <$$> es)
        Incr      a lv     -> Incr      (f a) (f <$> lv)
        Decr      a lv     -> Decr      (f a) (f <$> lv)
        PreIncr   a lv     -> PreIncr   (f a) (f <$> lv)
        PreDecr   a lv     -> PreDecr   (f a) (f <$> lv)
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

instance Overable Program        where overF = pAnot
instance Overable TopDef         where overF = toAnot
instance Overable ClassDef       where overF = cdAnot
instance Overable ClassPart      where overF = cpAnot
instance Overable ClassHierarchy where overF = chAnot
instance Overable StructDef      where overF = stAnot
instance Overable SField         where overF = sfAnot
instance Overable TypeDef        where overF = tdAnot
instance Overable FnDef          where overF = fAnot
instance Overable Arg            where overF = aAnot
instance Overable Block          where overF = bAnot
instance Overable Stmt           where overF = sAnot
instance Overable Item           where overF = iAnot
instance Overable Type           where overF = tAnot
instance Overable DimT           where overF = dtAnot
instance Overable Expr           where overF = eAnot
instance Overable LValue         where overF = lvAnot
instance Overable DimE           where overF = deAnot
instance Overable AddOp          where overF = addAnot
instance Overable MulOp          where overF = mAnot
instance Overable RelOp          where overF = rAnot

--------------------------------------------------------------------------------
-- Converting:
--------------------------------------------------------------------------------

convert :: J.Program a -> Program a
convert (J.Program anot fns)          = Program anot $ cto <$> fns
    where cto (J.TClassDef  a cd)     = TClassDef  a (ccd cd)
          cto (J.TTypeDef   a td)     = TTypeDef   a (ctd td)
          cto (J.TStructDef a sd)     = TStructDef a (csd sd)
          cto (J.TFnDef     a fd)     = TFnDef     a (cfd fd)
          ccd (J.ClassDef  a i h ps)  = ClassDef  a (ci i) (cch h) (ccp <$> ps)
          ccp (J.MethodDef a fd)      = MethodDef a (cfd fd)
          ccp (J.ClassProp a t i)     = ClassProp a (ct t) (ci i)
          cch (J.HBase     a)         = HBase     a
          cch (J.HExtend   a i)       = HExtend   a (ci i)
          ctd (J.TypeDef   a t i)     = TypeDef   a (ct t) (ci i)
          csd (J.StructDef a i sf)    = StructDef a (ci i)
                                                    (zipWith csf [0..] sf)
          csf i (J.SField  a t n)     = SField    a (ct t) (ci n) i
          
          cfd (J.FnDef a r i p b)     = FnDef     a (ct r) (ci i)
                                                    (ca <$> p) (cb b)
          ca  (J.Arg       a t i)     = Arg       a (ct t) (ci i)
          ct  (J.Int       a)         = Int       a
          ct  (J.Doub      a)         = Doub      a
          ct  (J.Bool      a)         = Bool      a
          ct  (J.Void      a)         = Void      a
          ct  (J.Array     a t ds)    = Array     a (ct t) (cdt <$> ds)
          ct  (J.TStruct   a i)       = TStruct   a (ci i)
          ct  (J.TRef      a i)       = TRef      a (ci i)
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
          cs  (J.Assign    a lv e)    = Assign    a (cLV lv) (ce e)
          cs  (J.Ret       a e)       = Ret       a (ce e)
          cs  (J.VRet      a)         = VRet      a
          cs  (J.Cond      a c i)     = Cond      a (ce c) (cs i)
          cs  (J.CondElse  a c i e)   = CondElse  a (ce c) (cs i) (cs e)
          cs  (J.While     a c i)     = While     a (ce c) (cs i)
          cs  (J.For       a t i e s) = For       a (ct t) (ci i) (ce e) (cs s)
          cs  (J.SExp      a e)       = SExp      a (ce e)
          ce  (J.ENew      a t des)   = ENew      a (ct t) (cde <$> des)
          ce  (J.EVar      a lv)      = EVar      a (cLV lv)
          ce  (J.ELitInt   a v)       = ELitInt   a v
          ce  (J.ELitDoub  a v)       = ELitDoub  a v
          ce  (J.EString   a v)       = EString   a v
          ce  (J.ELitTrue  a)         = ELitTrue  a
          ce  (J.ELitFalse a)         = ELitFalse a
          ce  (J.ECastNullX a x)      = ECastNull a $ TRef  a (ci x)
          ce  (J.ECastNullA a t d)    = ECastNull a $ Array a (ct t) (cdt <$> d)
          ce  (J.ECastNullS a s)      = ECastNull a $ TStruct a (ci s)
          ce  (J.EApp      a i es)    = EApp      a (ci i) (ce <$> es)
          ce  (J.EMApp     a e i es)  = EMApp     a (ce e) (ci i) (ce <$> es)
          ce  (J.Neg       a e)       = Neg       a (ce e)
          ce  (J.Not       a e)       = Not       a (ce e)
          ce  (J.Incr      a lv)      = Incr      a (cLV lv)
          ce  (J.Decr      a lv)      = Decr      a (cLV lv)
          ce  (J.PreIncr   a lv)      = PreIncr   a (cLV lv)
          ce  (J.PreDecr   a lv)      = PreDecr   a (cLV lv)
          ce  (J.EMul      a l o r)   = EMul      a (ce l) (cmo o) (ce r)
          ce  (J.EAdd      a l o r)   = EAdd      a (ce l) (cao o) (ce r)
          ce  (J.ERel      a l o r)   = ERel      a (ce l) (cro o) (ce r)
          ce  (J.EAnd      a l   r)   = EAnd      a (ce l)         (ce r)
          ce  (J.EOr       a l   r)   = EOr       a (ce l)         (ce r)
          cdt (J.DimenT    a)         = DimenT    a
          cde (J.DimenE    a e)       = DimenE    a (ce e)
          cLV                         = leftifyLVal . clv
          clv (J.LValueV   a i d)     = LValueV   a (ci i) (cde <$> d)
          clv (J.LValueS   a i d lv)  = LValueS   a
                                                  (LValueV a (ci i) (cde <$> d))
                                                  (clv lv)

leftifyLVal :: LValue a -> LValue a
leftifyLVal = rebuildLVal . reverse . flattenLVal
    where rebuildLVal = \case
            [ ]    -> error "rebuildLVal: can't rebuild empty LValue tree."
            [x]    -> x
            (x:xs) -> LValueS (_lvAnot x) (rebuildLVal xs) x
          flattenLVal lv = case lv of
            LValueV {}    -> [lv]
            LValueS _ l r -> flattenLVal l ++ flattenLVal r