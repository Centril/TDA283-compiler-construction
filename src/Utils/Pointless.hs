{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Utils.Pointless
Description : Combinators to ease writing common cases in pointfree style.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

Combinators to ease writing common cases in pointfree style.
-}

module Utils.Pointless where

-- | '.|': Compose an unary function with a binary function.
-- from: http://hackage.haskell.org/package/pointless-fun-1.1.0.5/docs/Data-Function-Pointless.html
(.|) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.|) f g x y = f $ g x y
infixr 7 .|

-- | '|.|': Compose a binary function with two unary functions.
(|.|) :: (c -> d -> e) -> ((a -> c), (b -> d)) -> a -> b -> e
(|.|) f (g, h) a b = f (g a) (h b)
infixr 8 |.|

-- | '|.': Compose a binary function with an unary function for both arguments.
(|.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(|.) f g a b = f (g a) (g b)
infixr 8 |.