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

module WSC.Parser where

import WSC.AST
import Text.Trifecta

program :: MonadParser m => m Prog
program = undefined

parseFile :: FilePath -> IO (Maybe Prog)
parseFile filePath = parseFromFile program filePath