{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module WSC.Driver where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import Data.Either

newtype WSCDriver e s a = WSCDriver { runWSCDriver :: ErrorT e (StateT s IO) a }
  deriving (Functor, Applicative, Monad, MonadPlus, MonadIO, MonadError e, MonadState s) -- hax

io :: Error e => IO a -> WSCDriver e s a
io = liftIO