{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Noblit
(
  Database,
  Error (..),
  MonadNoblit,
  connect,
  latest,
  query,
  transact,
)
where

import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Prelude hiding (Read)
import Data.Text (Text)

import qualified Data.Text as Text

import Database.Noblit.Query (Read, Write)
import Database.Noblit.Schema (TransactionId)
import Database.Noblit.Primitive (EntityId (..))

newtype Error = Error Text

instance Show Error where
  show (Error message) = Text.unpack message

newtype Database = Database Int -- TODO: Pointer to foreign db.

-- TODO: Remove MonadIO constraint, and provide a runNoblit function instead.
type MonadNoblit m = (MonadIO m, MonadError Error m, MonadReader Database m)

latest :: MonadNoblit m => m TransactionId
latest =
  -- TODO: Actually get transaction id from database.
  pure (EntityId 0)

query :: MonadNoblit m => TransactionId -> Read a -> m [a]
query _atTransaction readOnlyQuery = do
  -- TODO: Actually run query against database.
  liftIO $ putStrLn $ show readOnlyQuery
  error "TODO: implement"

transact :: MonadNoblit m => TransactionId -> Write a -> m ([a], TransactionId)
transact _assumedLatest _write = error "TODO: implement"

connect :: (MonadIO m, MonadError Error m) => m Database
connect = pure $ Database 0 -- TODO: Set up real connection.
