{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module WSC.Main where

import Control.Monad
import Data.Maybe
import System.Console.CmdArgs
import System.Exit
import System.IO.Error
import WSC.Parser (parseFile)

data WSCFlags = WSCFlags
  { dumpAst :: Bool
  , file    :: FilePath
  , outfile :: FilePath
  } deriving (Eq, Show, Data, Typeable)

wscFlags = WSCFlags 
  { dumpAst = False
           &= name "dump-ast" 
           &= help "Dump a textual representation of the AST."
  , file    = def
           &= args
           &= typFile
  , outfile = def
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
  when (dumpAst args) $ do
    print ast
  exitSuccess