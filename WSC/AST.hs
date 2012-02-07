-----------------------------------------------------------------------------
-- |
-- Module      : WSC.AST 
-- Copyright   : (c) 2012 Dylan Lukes 
-- License     : BSD3
-- Maintainer  : lukes.dylan+wsc@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The representation of the abstract syntax tree for WSC source code. 
-----------------------------------------------------------------------------
module WSC.AST ( Var
               , Con
               , Arity
               , Atom(..)
               , Lit(..)
               , Expr(..)
               , Alt(..)
               , Obj(..)
               , PrimOp(..)
               , Decl(..)
               , Prog(..) ) where

import Data.ByteString

-- | Variable identifiers.
type Var = ByteString
-- | Constructor identifiers.
type Con = ByteString

-- | Arity, which may or may not be statically known (!).
type Arity = Maybe Int

-- | Atomic (sub)expressions.
data Atom
  = LitAtom Lit
  | VarAtom Var
  deriving (Eq, Show)

-- | Literals. Currently only integers.
data Lit
  = IntLit Integer
  deriving (Eq, Show)

-- | Expressions. They is what they is.
data Expr
  = AtomExpr   Atom
  | FunAppExpr Arity Var [Atom]
  | PrimOpExpr PrimOp [Atom]
  | LetExpr    [Decl] Expr
  | CaseExpr   Expr Var [Alt]
  deriving (Eq, Show)

-- | Alternatives for a 
data Alt
  = AlgAlt Con [Var] Expr
  -- ^ Default pattern. No `Var' is needed since the scrutinee is bound in the CaseExpr.
  | DefaultAlt Expr
  deriving (Eq, Show)

data Obj
  = FunObj [Var] Expr
  | PapObj Var [Atom]
  | ConObj Con [Atom]
  | ThunkObj Expr
  | BlackHoleObj
  deriving (Eq, Show)

-- | Primitive operations.
data PrimOp
  = AddOp | SubOp | MulOp
  | EqOp  | LtOp  | GtOp
  | LteOp | GteOp
  | IntToBoolOp
  deriving (Eq, Show)

argCount :: PrimOp -> Int
argCount AddOp       = 2
argCount SubOp       = 2
argCount MulOp       = 2
argCount EqOp        = 2
argCount LtOp        = 2
argCount GtOp        = 2
argCount LteOp       = 2
argCount GteOp       = 2
argCount IntToBoolOp = 1

-- | Declarations (Bindings).
data Decl = Decl Var Obj deriving (Eq, Show)

-- | Whole programs.
data Prog = Prog [Decl] deriving (Eq, Show)

