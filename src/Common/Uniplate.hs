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
Module      : Common.Uniplate
Description : Data and Typeable instances for Javalette AST.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Data and Typeable instances for Javalette AST.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
module Common.Uniplate where

import Data.Data

import Javalette.Abs

deriving instance Data     Ident
deriving instance Typeable Ident
deriving instance Data     a => Data     (Type    a)
deriving instance Typeable a => Typeable (Type    a)
deriving instance Data     a => Data     (Arg     a)
deriving instance Typeable a => Typeable (Arg     a)
deriving instance Data     a => Data     (RelOp   a)
deriving instance Typeable a => Typeable (RelOp   a)
deriving instance Data     a => Data     (MulOp   a)
deriving instance Typeable a => Typeable (MulOp   a)
deriving instance Data     a => Data     (AddOp   a)
deriving instance Typeable a => Typeable (AddOp   a)
deriving instance Data     a => Data     (Expr    a)
deriving instance Typeable a => Typeable (Expr    a)
deriving instance Data     a => Data     (Item    a)
deriving instance Typeable a => Typeable (Item    a)
deriving instance Data     a => Data     (Stmt    a)
deriving instance Typeable a => Typeable (Stmt    a)
deriving instance Data     a => Data     (Block   a)
deriving instance Typeable a => Typeable (Block   a)
deriving instance Data     a => Data     (TopDef  a)
deriving instance Typeable a => Typeable (TopDef  a)
deriving instance Data     a => Data     (Program a)
deriving instance Typeable a => Typeable (Program a)