{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module WSC.Main where

import Control.Monad
import Data.Maybe
import System.Console.CmdArgs
import System.Exit
import System.IO.Error
import Text.Groom
import WSC.Parser (parseFile)

data WSCFlags = WSCFlags
  { printAst      :: Bool
  , printSymTable :: Bool
  , file         :: FilePath
  , outfile      :: FilePath
  } deriving (Eq, Show, Data, Typeable)

wscFlags = WSCFlags 
  { printAst
      = False
     &= name "print-ast" 
     &= help "Print the AST."
  , printSymTable
      = False
     &= name "print-symtable"
     &= help "Print the symbol table."
  , file    
      = def
     &= args
     &= typFile
  , outfile 
      = def
     &= typFile
     &= help "Destination of output file."
  } &= program "wsc"
    &= summary "Winchester STG Compiler v0.0, (c) Dylan Lukes 2012"

main = do
  args <- cmdArgs wscFlags
  ast  <- parseFile (file args)
  when (isNothing ast) $ do
    print "Fail: Parsing failed."
    exitFailure
  when (printAst args) $ do
    putStrLn . groom $ ast
  exitSuccess