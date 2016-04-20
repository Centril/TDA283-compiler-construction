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
Module      : Frontend.Example
Description : Type checker for Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Example function for the typechecker.
-}
module Frontend.Example where

import Javalette.Abs

simpleBlock :: Block
simpleBlock = Block
    [
        Decl Int [Init (Ident "i") (ELitInt 1), Init (Ident "a") (ELitInt 2)],
        Decl Int [Init (Ident "j") (ELitInt 2)],
        Ret (EVar (Ident "i"))
    ]

simpleProgram :: Program
simpleProgram = Program [
    FnDef Int (Ident "main") [] (
        Block
        [
            Decl Int [Init (Ident "a") (ELitInt 1),
            Init (Ident "b") (ELitInt 2)],
            Decl Int [Init (Ident "c") (ELitInt 3)],
            Ret (EVar (Ident "c"))
        ]
    )]

largeBlock :: Program
largeBlock = Program [FnDef Doub (Ident "test") [] (Block
    [
        Decl Int [Init (Ident "l") (ELitInt 1)],
        While ELitTrue (BStmt (Block
        [
            Decl Int [Init (Ident "a") (ELitInt 1)],
            Decl Int [Init (Ident "b") (ELitInt 2)],
            Decl Int [Init (Ident "c") (ELitInt 3)]
    ]))])]

returnCheckTest :: [Expr]
returnCheckTest =
        [(ELitInt 1),
        (EAdd (EVar (Ident "a")) Minus (ELitInt 1)),
        ELitTrue,
        ELitFalse,
        (ERel (ELitInt 1) GTH (ELitInt 1)),
        (ERel (ELitDoub 1.5) EQU (ELitDoub 1.5)),
        (ERel (EVar (Ident "a")) LTH (EVar (Ident "b"))),
        (EAnd ELitTrue ELitTrue),
        (EOr ELitTrue ELitFalse),
        (Not ELitTrue),
        (ERel (Neg (ELitInt 1)) LTH (ELitInt 5)),
        (ERel (EMul (ELitInt 5) Times (ELitInt 5)) EQU (ELitInt 25)),
        (ERel (EAdd (ELitInt 5) Plus (ELitInt 1)) LTH (ELitInt 7)),
        (ERel (EMul (ELitInt 5) Mod (ELitInt 5)) EQU (ELitInt 0)),
        (ERel (EMul (ELitInt 15) Div (ELitInt 5)) EQU (ELitInt 3)),
        (ERel (EMul (ELitInt 15) Div (ELitInt 0)) EQU (ELitInt 3))]

