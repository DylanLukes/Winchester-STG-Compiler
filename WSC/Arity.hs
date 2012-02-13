-----------------------------------------------------------------------------
-- |
-- Module      : WSC.Arity 
-- Copyright   : (c) 2012 Dylan Lukes 
-- License     : BSD3
-- Maintainer  : lukes.dylan+wsc@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Routines for filling in the arities of function applications.
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module WSC.Arity where

import Control.Monad
import Control.Monad.Error
import Control.Monad.RWS.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (foldl')

import WSC.AST
import WSC.Driver

type SymbolTable = HashMap Var Int -- R

removeVars :: [Var] -> SymbolTable -> SymbolTable
removeVars vars symtab = foldl' (flip HashMap.delete) symtab vars 

resolveArities :: ArityAnalysis s => Pass String s
resolveArities ast = case runRWS (arityAnalysis ast) mempty () of
  (ast', _, Nothing)  -> Right ast'
  (_,    _, Just err) -> Left err

class ArityAnalysis a where
  arityAnalysis :: a -> RWS SymbolTable (Maybe String) () a

instance ArityAnalysis Expr where
  arityAnalysis = undefined

instance ArityAnalysis Obj where
  arityAnalysis (FunObj args body) = do
    body' <- local
      (removeVars args)
      (arityAnalysis body)
    return (FunObj args body')

instance ArityAnalysis Decl where
  arityAnalysis (Decl var obj) = do
    obj' <- arityAnalysis obj
    return (Decl var obj')

instance ArityAnalysis Prog where
  arityAnalysis (Prog decls) = do
    let globals = HashMap.fromList
          [ (var, length args) | Decl var (FunObj args _) <- decls ]
    decls' <- local 
      (mappend globals)
      (arityAnalysis `mapM` decls)
    return (Prog decls')