module WSC.AST.Annotation where

data Annotation = Annotation
  { 
  } deriving (Eq, Show)

emptyAnnotation :: Annotation
emptyAnnotation = Annotation { }