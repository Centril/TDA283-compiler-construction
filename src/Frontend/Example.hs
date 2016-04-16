{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Frontend.Example
Description : Type checker for Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

Example function for the typechecker.
-}
module Frontend.Example where

import Javalette.Abs

import Frontend.Types

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
