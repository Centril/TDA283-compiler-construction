module Frontend.TypeCheck where

import Data.Map (Map)
import qualified Data.Map as Map

import Javalette.Lex
import Javalette.Par
import Javalette.Skel
import Javalette.Print
import Javalette.Abs
import Javalette.ErrM

type Env =     (Sig, [Context])         -- functions and context stack
type Sig =     Map Ident ([Type], Type) -- function type signature
type Context = Map Ident Type           -- variables with their types

-- infer :: Env -> Expr -> Type

-- check :: Env -> Expr -> Type -> Err Bool?

-- check :: Env -> [Stmt] -> Err Bool?

-- check :: Env -> Def -> Err Bool?

-- check :: Program -> Err Bool?

-- lookupVar :: Env -> Ident -> Err Type

-- lookupFun :: Env -> Ident -> Err ([Type],Type)

-- updateVar :: Env -> Ident -> Type -> Err Env
-- extendVar needed?

-- updateFun :: Env -> Ident -> ([Type],Type) -> Err Env
-- extendFun needed?

-- newBlock  :: Env -> Env

-- emptyEnv  :: Env

typeCheck :: Program -> Err Program
typeCheck s = Bad "Not implemented!"
