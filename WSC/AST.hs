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

instance HasAnnotation Atom where
  getAnnotation (LitAtom ann _) = ann
  getAnnotation (VarAtom ann _) = ann

  setAnnotation ann (LitAtom _ lit) = LitAtom ann lit
  setAnnotation ann (VarAtom _ var) = VarAtom ann var

-- | Literals. Currently only integers.
data Lit
  = IntLit Annotation Integer
  deriving (Eq, Show)

instance HasAnnotation Lit where
  getAnnotation (IntLit ann _) = ann
  setAnnotation ann (IntLit _ int) = IntLit ann int 

-- | Expressions. They is what they is.
data Expr
  = AtomExpr   Annotation Atom
  | FunAppExpr Annotation Arity Var [Atom]
  | PrimOpExpr Annotation PrimOp [Atom]
  | LetExpr    Annotation [Decl] Expr
  | CaseExpr   Annotation Expr (Maybe Var) [Alt]
  deriving (Eq, Show)

instance HasAnnotation Expr where
  getAnnotation (AtomExpr   ann _)     = ann
  getAnnotation (FunAppExpr ann _ _ _) = ann
  getAnnotation (PrimOpExpr ann _ _)   = ann
  getAnnotation (LetExpr    ann _ _)   = ann
  getAnnotation (CaseExpr   ann _ _ _) = ann

  setAnnotation ann (AtomExpr _ atom )       = AtomExpr ann atom
  setAnnotation ann (FunAppExpr _ a f args)  = FunAppExpr ann a f args
  setAnnotation ann (PrimOpExpr _ op args)   = PrimOpExpr ann op args
  setAnnotation ann (LetExpr _ bnds body)    = LetExpr ann bnds body
  setAnnotation ann (CaseExpr _ expr s alts) = CaseExpr ann expr s alts 

-- | Alternatives for a 
data Alt
  = AlgAlt     Annotation Con [Var] Expr
  -- ^ Default pattern. No `Var' is needed since the scrutinee is bound in the CaseExpr.
  | DefaultAlt Annotation Expr
  deriving (Eq, Show)

instance HasAnnotation Alt where
  getAnnotation (AlgAlt ann _ _ _) = ann
  getAnnotation (DefaultAlt ann _) = ann

  setAnnotation ann (AlgAlt _ con vars expr) = AlgAlt ann con vars expr 
  setAnnotation ann (DefaultAlt _ expr)      = DefaultAlt ann expr

data Obj
  = FunObj       Annotation [Var] Expr
  | PapObj       Annotation Var [Atom]
  | ConObj       Annotation Con [Atom]
  | ThunkObj     Annotation Expr
  | BlackHoleObj Annotation
  deriving (Eq, Show)

instance HasAnnotation Obj where
  getAnnotation (FunObj ann _ _)   = ann
  getAnnotation (PapObj ann _ _)   = ann
  getAnnotation (ConObj ann _ _)   = ann
  getAnnotation (ThunkObj ann _)   = ann
  getAnnotation (BlackHoleObj ann) = ann

  setAnnotation ann (FunObj _ vars body) = FunObj ann vars body 
  setAnnotation ann (PapObj _ f args)    = PapObj ann f args
  setAnnotation ann (ConObj _ con args)  = ConObj ann con args
  setAnnotation ann (ThunkObj _ expr)    = ThunkObj ann expr
  setAnnotation ann (BlackHoleObj _)     = BlackHoleObj ann

isFun :: Obj -> Bool
isFun FunObj{} = True
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

instance HasAnnotation Decl where
  getAnnotation (Decl ann _ _) = ann
  setAnnotation ann (Decl _ var obj) = Decl ann var obj 

-- | Whole programs.
data Prog = Prog Annotation [Decl] deriving (Eq, Show)

instance HasAnnotation Prog where
  getAnnotation (Prog ann _) = ann
  setAnnotation ann (Prog _ decls) = Prog ann decls 

