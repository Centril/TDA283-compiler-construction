{- Javalette Compiler
   Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -}

{-|
Module      : Frontend.ReturnCheck
Description : General monadic, applicative, functor utility functions.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
Stability   : experimental
Portability : ALL

General monadic, applicative, functor utility functions.
-}

module Utils.Monad (
    -- * Operations
    (<!>), (<:>)
) where

import Control.Monad

-- | '<!>': sequential application of a non-applicative value
-- lifted into the same 'Applicative' of as the function applied.
(<!>) :: Applicative f => f (a -> b) -> a -> f b
(<!>) f = (<*>) f . pure
infixr 7 <!>

-- | '<!>': version of 'fmap' where non-applicative value is first lifed
-- purely into the 'Applicative'.
(<:>) :: Applicative f => (a -> b) -> a -> f b
(<:>) f = fmap f . pure
infixr 8 <:>