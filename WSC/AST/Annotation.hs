{-# LANGUAGE TemplateHaskell #-}
module WSC.AST.Annotation where

import Prelude hiding ((.))
import Control.Category

import Data.Lens.Common
import Data.Lens.Template

import Text.Trifecta.Parser.Class
import Text.Trifecta.Diagnostic.Rendering.Span

class HasAnnotation a where
  getAnnotation :: a -> Annotation
  setAnnotation :: Annotation -> a -> a

  annotation :: Lens a Annotation
  annotation = lens getAnnotation setAnnotation

data Annotation = Annotation
  { _annSpan :: Maybe Span
  } deriving (Eq, Show)

$(makeLens ''Annotation)

emptyAnnotation :: Annotation
emptyAnnotation = Annotation
  { _annSpan = Nothing
  }

annotateSpan :: (MonadParser m, HasAnnotation a) => m a -> m a
annotateSpan p = do
  (a :~ spn) <- spanned p
  return $ setL (annSpan . annotation) (Just spn) a