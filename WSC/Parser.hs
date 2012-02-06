-----------------------------------------------------------------------------
-- |
-- Module      : WSC.Parser 
-- Copyright   : (c) 2012 Dylan Lukes 
-- License     : BSD3
-- Maintainer  : lukes.dylan+wsc@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Trifecta parser combinators for parsing WSC STG source code.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module WSC.Parser where

import Control.Applicative
import Data.Monoid
import Text.Trifecta
import Text.Trifecta.Highlight.Prim as Highlight
import Text.Trifecta.Parser.Identifier.Style
import WSC.AST
import WSC.Util

{- Lexeme Parsers -}

var :: MonadParser m => m Var
var = lexeme . ident $ IdentifierStyle
  { styleName      = "identifier"
  , styleStart     = () <$ (lower <|> char '_')
  , styleLetter    = () <$ (alphaNum <|> oneOf "_'#")
  , styleReserved  = set ["add#", "sub#", "mul#"
                         , "eq#", "lt#", "gt#", "lte#", "gte#"
                         , "intToBool#"
                         ]
  , styleHighlight         = Highlight.Identifier
  , styleReservedHighlight = Highlight.ReservedIdentifier
  }

con :: MonadParser m => m Con
con = lexeme . ident $ IdentifierStyle
  { styleName      = "constructor"
  , styleStart     = () <$ (upper <|> char '_')
  , styleLetter    = () <$ (letter <|> oneOf "_#")
  , styleReserved  = mempty
  , styleHighlight         = Highlight.Constructor
  , styleReservedHighlight = Highlight.ReservedConstructor
  }

{- Term Parsers -}

atom :: MonadParser m => m Atom
atom =  litAtom
    <|> varAtom
  where litAtom = LitAtom <$> lit
        varAtom = VarAtom <$> var

lit :: MonadParser m => m Lit
lit = intLit
  where intLit = IntLit <$> integer

expr :: MonadParser m => m Expr
expr =  funAppExpr 
    <|> primOpExpr
    <|> atomExpr
    <?> "expression"
  where atomExpr   = AtomExpr <$> atom
        funAppExpr = FunAppExpr Nothing <$> var <*> many atom
        primOpExpr = PrimOpExpr <$> primOp <*> many atom

obj :: MonadParser m => m Obj
obj =  funObj
   <|> papObj
   <|> conObj
   <|> thunkObj
   <|> blackHoleObj
  where funObj = symbol "FUN" *> parens (FunObj <$> many var <* symbol "->" <*> expr)
        papObj = symbol "PAP" *> parens (PapObj <$> var <*> many atom)
        conObj = symbol "CON" *> parens (ConObj <$> con <*> many atom)
        thunkObj     = ThunkObj <$ symbol "THUNK" <*> parens (expr)
        blackHoleObj = BlackHoleObj <$ symbol "BLACKHOLE"

primOp :: MonadParser m => m PrimOp
primOp =  AddOp <$ symbol "add#" 
      <|> SubOp <$ symbol "sub#"
      <|> MulOp <$ symbol "mul#"
      <|> EqOp  <$ symbol "eq#"
      <|> LtOp  <$ symbol "lt#"
      <|> GtOp  <$ symbol "gt#"
      <|> LteOp <$ symbol "lte#"
      <|> GteOp <$ symbol "gte#"
      <|> IntToBoolOp <$ symbol "intToBool#"

decl :: MonadParser m => m Decl
decl = Decl <$> var <* symbol "=" <*> obj

prog :: MonadParser m => m Prog
prog = Prog <$> decl `sepEndBy` semi

parseFile :: FilePath -> IO (Maybe Prog)
parseFile filePath = parseFromFile prog filePath