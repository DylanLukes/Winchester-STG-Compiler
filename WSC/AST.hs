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
module WSC.AST ( Var(..)
               , Con(..)
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
import WSC.AST.Annotation

-- | Variable identifiers.
type Var = ByteString
-- | Constructor identifiers.
type Con = ByteString

-- | Arity, which may or may not be statically known (!).
type Arity = Maybe Int

-- | Atomic (sub)expressions.
data Atom
  = LitAtom Annotation Lit
  | VarAtom Annotation Var
  deriving (Eq, Show)

-- | Literals. Currently only integers.
data Lit
  = IntLit Annotation Integer
  deriving (Eq, Show)

-- | Expressions. They is what they is.
data Expr
  = AtomExpr   Annotation Atom
  | FunAppExpr Annotation Arity Var [Atom]
  | PrimOpExpr Annotation PrimOp [Atom]
  | LetExpr    Annotation [Decl] Expr
  | CaseExpr   Annotation Expr (Maybe Var) [Alt]
  deriving (Eq, Show)

-- | Alternatives for a 
data Alt
  = AlgAlt     Annotation Con [Var] Expr
  -- ^ Default pattern. No `Var' is needed since the scrutinee is bound in the CaseExpr.
  | DefaultAlt Annotation Expr
  deriving (Eq, Show)

data Obj
  = FunObj       Annotation [Var] Expr
  | PapObj       Annotation Var [Atom]
  | ConObj       Annotation Con [Atom]
  | ThunkObj     Annotation Expr
  | BlackHoleObj Annotation
  deriving (Eq, Show)

isFun :: Obj -> Bool
isFun (FunObj _ _ _) = True
isFun _ = False

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
data Decl = Decl Annotation Var Obj deriving (Eq, Show)

-- | Whole programs.
data Prog = Prog Annotation [Decl] deriving (Eq, Show)

