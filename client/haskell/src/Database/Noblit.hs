{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Noblit
(
  Error (..),
  MonadNoblit (..),
)
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT)
import Prelude hiding (Read)
import Data.Text (Text)

import qualified Data.Text as Text

import Database.Noblit.Query (Read, Write)
import Database.Noblit.Schema (TransactionId)

newtype Error = Error Text

instance Show Error where
  show (Error message) = Text.unpack message

class MonadError Error m => MonadNoblit m where
  latest :: m TransactionId
  query :: TransactionId -> Read a -> m [a]
  transact :: TransactionId -> Write a -> m ([a], TransactionId)

instance MonadIO m => MonadNoblit (ExceptT Error m) where
  latest = error "TODO: implement"
  query _atTransaction _query = error "TODO: implement"
  transact _assumedLatest _write = error "TODO: implement"
