{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Database.Noblit.Schema (Attribute)
import Database.Noblit.Query (Clause, Query)
import Database.Noblit.Primitive (EntityId)
import Data.Text (Text)
import Data.Word (Word64)

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
  => (forall a. Clause a -> q a)
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
  f $ Schema
    <$> Builtins.attribute userName      ("user.name"      :: Text) typeString unique    single
    <*> Builtins.attribute issueTitle    ("issue.title"    :: Text) typeString unique    single
    <*> Builtins.attribute issueBody     ("issue.body"     :: Text) typeString nonUnique single
    <*> Builtins.attribute issuePriority ("issue.priority" :: Text) typeUint64 nonUnique single
    <*> Builtins.attribute issueAuthor   ("issue.author"   :: Text) typeRef    nonUnique single
    <*> Builtins.attribute commentBody   ("comment.body"   :: Text) typeString nonUnique single
    <*> Builtins.attribute commentAuthor ("comment.author" :: Text) typeRef    nonUnique single
    <*> Builtins.attribute issueComment  ("issue.comment"  :: Text) typeRef    nonUnique many

main :: IO ()
main = putStrLn ""
