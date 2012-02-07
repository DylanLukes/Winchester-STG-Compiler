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
import Data.ByteString
import Data.Monoid
import Text.Trifecta hiding (semi)
import qualified Text.Trifecta as Trifecta (semi)
import Text.Trifecta.Highlight.Prim as Highlight
import Text.Trifecta.Parser.Identifier.Style
import WSC.AST
import WSC.Util

-- This is annoying. Make sure edwardk fixes it...
semi :: MonadParser m => m Char
semi = lexeme Trifecta.semi

{- Lexeme Parsers -}

varIdStyle :: MonadParser m => IdentifierStyle m
varIdStyle = IdentifierStyle
  { styleName      = "identifier"
  , styleStart     = () <$ (lower <|> char '_')
  , styleLetter    = () <$ (alphaNum <|> oneOf "_'#")
  , styleReserved  = set [ "let", "in", "case", "of"
                         , "_", "->"
                         , "add#", "sub#", "mul#"
                         , "eq#", "lt#", "gt#", "lte#", "gte#"
                         , "intToBool#"
                         ]
  , styleHighlight         = Highlight.Identifier
  , styleReservedHighlight = Highlight.ReservedIdentifier
  }

var :: MonadParser m => m Var
var = lexeme (ident varIdStyle)

kw :: MonadParser m => ByteString -> m ()
kw s = lexeme (reserveByteString varIdStyle s)

con :: MonadParser m => m Con
con = lexeme . ident $ IdentifierStyle
  { styleName      = "constructor"
  , styleStart     = () <$ (upper <|> char '_')
  , styleLetter    = () <$ (letter <|> oneOf "_#")
  , styleReserved  = set [ "FUN", "PAP", "CON", "BLACKHOLE"]
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
expr =  try funAppExpr 
    <|> primOpExpr
    <|> atomExpr
    <|> letExpr
    <|> caseExpr
    <?> "expression"
  where atomExpr   = AtomExpr <$> atom
        funAppExpr = FunAppExpr Nothing <$> var <*> some atom
        primOpExpr = PrimOpExpr <$> primOp <*> many atom
        letExpr    = LetExpr <$ kw "let" <*> braces (decl `sepEndBy1` semi) <* kw "in" <*> expr
        caseExpr   = CaseExpr <$ kw "case" <*> expr <* kw "of" <*> var <*> braces (alt `sepEndBy1` semi)

alt :: MonadParser m => m Alt
alt =  algAlt
   <|> defaultAlt
   <?> "alternative"
  where algAlt = AlgAlt <$> con <*> many var <* kw "->" <*> expr
        defaultAlt = DefaultAlt <$ kw "_" <* kw "->" <*> expr

obj :: MonadParser m => m Obj
obj =  funObj
   <|> papObj
   <|> conObj
   <|> thunkObj
   <|> blackHoleObj
  where funObj = kw "FUN" *> parens (FunObj <$> many var <* symbol "->" <*> expr)
        papObj = kw "PAP" *> parens (PapObj <$> var <*> many atom)
        conObj = kw "CON" *> parens (ConObj <$> con <*> many atom)
        thunkObj     = ThunkObj <$ kw "THUNK" <*> parens (expr)
        blackHoleObj = BlackHoleObj <$ kw "BLACKHOLE"

primOp :: MonadParser m => m PrimOp
primOp =  AddOp <$ kw "add#" 
      <|> SubOp <$ kw "sub#"
      <|> MulOp <$ kw "mul#"
      <|> EqOp  <$ kw "eq#"
      <|> LtOp  <$ kw "lt#"
      <|> GtOp  <$ kw "gt#"
      <|> LteOp <$ kw "lte#"
      <|> GteOp <$ kw "gte#"
      <|> IntToBoolOp <$ kw "intToBool#"

decl :: MonadParser m => m Decl
decl = Decl <$> var <* symbol "=" <*> obj

prog :: MonadParser m => m Prog
prog = Prog <$> decl `sepEndBy` semi

parseFile :: FilePath -> IO (Maybe Prog)
parseFile filePath = parseFromFile prog filePath