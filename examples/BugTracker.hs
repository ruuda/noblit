{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Database.Noblit.Schema (Attribute)
import Database.Noblit.Query (Clause, Query)
import Database.Noblit.Primitive (EntityId)
import Data.Text (Text)
import Data.Word (Word64)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)

import Database.Noblit (MonadNoblit)

import qualified Database.Noblit as Noblit
import qualified Database.Noblit.Query as Noblit
import qualified Database.Noblit.Builtins as Builtins

-- Domain specific code below.

data Schema a = Schema
  { attrUserName      :: a Text
  , attrIssueTitle    :: a Text
  , attrIssueBody     :: a Text
  , attrIssuePriority :: a Word64
  , attrIssueAuthor   :: a EntityId
  , attrCommentBody   :: a Text
  , attrCommentAuthor :: a EntityId
  , attrIssueComment  :: a EntityId
  }

-- Construct a query for the bug tracker database schema.
-- The function `f` controls the type of query. If we merely
-- want to retrieve the attribute ids of the attributes used
-- in the schema at startup time, then we would do a read, but
-- if we need to create the initial schema, then it could be a
-- write.
schema
  :: Query q
  => (forall a. Clause a -> q a)
  -> q (Schema Attribute)
schema f = do
  -- Get the built-in data types so we can use them in attributes.
  datatypes <- Builtins.datatypes
  let
    typeString = Builtins.typeString datatypes
    typeUint64 = Builtins.typeUint64 datatypes
    typeRef    = Builtins.typeRef datatypes

  userName      <- Noblit.variable
  issueTitle    <- Noblit.variable
  issueBody     <- Noblit.variable
  issuePriority <- Noblit.variable
  issueAuthor   <- Noblit.variable
  commentBody   <- Noblit.variable
  commentAuthor <- Noblit.variable
  issueComment  <- Noblit.variable

  let
    -- db.attribute.unique and db.attribute.many
    -- have boolean type. I could define ADTs and
    -- implement Value for them though.
    unique    = True
    nonUnique = False
    many      = True
    single    = False

  -- We might be asserting, or we might be selecting.
  f $ Schema
    <$> Builtins.isAttribute userName      ("user.name"      :: Text) typeString unique    single
    <*> Builtins.isAttribute issueTitle    ("issue.title"    :: Text) typeString unique    single
    <*> Builtins.isAttribute issueBody     ("issue.body"     :: Text) typeString nonUnique single
    <*> Builtins.isAttribute issuePriority ("issue.priority" :: Text) typeUint64 nonUnique single
    <*> Builtins.isAttribute issueAuthor   ("issue.author"   :: Text) typeRef    nonUnique single
    <*> Builtins.isAttribute commentBody   ("comment.body"   :: Text) typeString nonUnique single
    <*> Builtins.isAttribute commentAuthor ("comment.author" :: Text) typeRef    nonUnique single
    <*> Builtins.isAttribute issueComment  ("issue.comment"  :: Text) typeRef    nonUnique many

noblitMain :: MonadNoblit m => m Text
noblitMain = do
  db <- Noblit.latest
  result <- Noblit.query db (schema Noblit.where_)
  case result of
    [_scm] -> pure "Got single schema."
    _      -> pure "Got unexpected number of results."

main :: IO ()
main = do
  result <- runExceptT $ do
    database <- Noblit.connect
    runReaderT noblitMain database

  case result of
    Left err  -> putStrLn $ (show err)
    Right msg -> putStrLn $ (show msg)
