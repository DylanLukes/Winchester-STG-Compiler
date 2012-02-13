{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module WSC.Main where

import Control.Monad.Error
import Control.Monad.State

import System.Console.CmdArgs.Implicit
import System.Exit

import Text.Groom
import Text.Printf

import WSC.AST as AST
import WSC.Arity
import WSC.Driver
import WSC.Parser (parseFile)

data WSCFlags = WSCFlags
  { printAst      :: Bool
  , printSymTable :: Bool
  , file          :: FilePath
  , outfile       :: FilePath
  } deriving (Eq, Show, Data, Typeable)

flags = WSCFlags 
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

driver :: WSCDriver String AST.Prog ExitCode
driver = let 
  run = do
    args <- io $ cmdArgs flags

    io (parseFile $ file args) >>=
      maybe 
        (throwError $ strMsg "Could not parse file.")
        put

    when (printAst args) $ do
      ast <- get
      io $ putStrLn (groom ast)

    pass resolveArities

    return ExitSuccess

  in catchError run $ \e -> do
    io $ print e
    return (ExitFailure (-1))

main = do
  -- this undefined will be promptly replaced by `put'
  r <- evalStateT (runErrorT (runWSCDriver driver)) (undefined)
  case r of
    Left err -> do
      printf "Unhandled error in driver: %s" (show err)
      exitFailure 
    Right ec -> exitWith ec
