{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Noblit.Schema (Attribute)
import Database.Noblit.Query (Clause, Query)
import Database.Noblit.Primitive (EntityId)
import Data.Text (Text)
import Data.Word (Word64)

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
-- in the schema at startup time,
schema
  :: Query q
  => (Clause a -> q a)
  -> q (Schema Attribute)
schema f = do
  dbTypeString  <- Noblit.variable
  dbTypeUint64  <- Noblit.variable
  dbTypeRef     <- Noblit.variable
  userName      <- Noblit.variable
  issueTitle    <- Noblit.variable
  issueBody     <- Noblit.variable
  issuePriority <- Noblit.variable
  issueAuthor   <- Noblit.variable
  commentBody   <- Noblit.variable
  commentAuthor <- Noblit.variable
  issueComment  <- Noblit.variable

  Noblit.where_ $ do
    Noblit.triplet dbTypeString Builtins.typeName "string"
    Noblit.triplet dbTypeUint64 Builtins.typeName "uint64"
    Noblit.triplet dbTypeRef    Builtins.typeName "ref"

  let
    -- db.attribute.unique and db.attribute.many
    -- have boolean type. I could define ADTs and
    -- implement Value for them though.
    unique    = True
    nonUnique = False
    many      = True
    single    = False

  -- We might be asserting, or we might be selecting.
  f $ do
    attrUserName      <- Builtins.attribute userName      "user.name"      dbTypeString unique    single
    attrIssueTitle    <- _ -- Builtins.attribute issueTitle    "issue.title"    dbTypeString unique    single
    attrIssueBody     <- _ -- Builtins.attribute issueBody     "issue.body"     dbTypeString nonUnique single
    attrIssuePriority <- _ -- Builtins.attribute issuePriority "issue.priority" dbTypeUint64 nonUnique single
    attrIssueAuthor   <- _ -- Builtins.attribute issueAuthor   "issue.author"   dbTypeRef    nonUnique single
    attrCommentBody   <- _ -- Builtins.attribute commentBody   "comment.body"   dbTypeString nonUnique single
    attrCommentAuthor <- _ -- Builtins.attribute commentAuthor "comment.author" dbTypeRef    nonUnique single
    attrIssueComment  <- _ -- Builtins.attribute issueComment  "issue.comment"  dbTypeRef    nonUnique many

    Schema
      <$> attrUserName
      <*> attrIssueTitle
      <*> attrIssueBody
      <*> attrIssuePriority
      <*> attrIssueAuthor
      <*> attrCommentBody
      <*> attrCommentAuthor
      <*> attrIssueComment

main :: IO ()
main = putStrLn ""
