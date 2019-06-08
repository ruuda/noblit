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

  (typeString, typeUint64, typeRef) <- Noblit.where_ $ do
    a <- Builtins.typeString dbTypeString
    b <- Builtins.typeUint64 dbTypeUint64
    c <- Builtins.typeRef dbTypeRef
    pure (a, b, c)

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
    attrUserName      <- Builtins.attribute userName      "user.name"      typeString unique    single
    attrIssueTitle    <- Builtins.attribute issueTitle    "issue.title"    typeString unique    single
    attrIssueBody     <- Builtins.attribute issueBody     "issue.body"     typeString nonUnique single
    attrIssuePriority <- Builtins.attribute issuePriority "issue.priority" typeUint64 nonUnique single
    attrIssueAuthor   <- Builtins.attribute issueAuthor   "issue.author"   typeRef    nonUnique single
    attrCommentBody   <- Builtins.attribute commentBody   "comment.body"   typeString nonUnique single
    attrCommentAuthor <- Builtins.attribute commentAuthor "comment.author" typeRef    nonUnique single
    attrIssueComment  <- Builtins.attribute issueComment  "issue.comment"  typeRef    nonUnique many

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
