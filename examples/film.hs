{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where


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
  dbTypeString  <- variable
  dbTypeUint64  <- variable
  dbTypeRef     <- variable
  userName      <- variable
  issueTitle    <- variable
  issueBody     <- variable
  issueAuthor   <- variable
  commentBody   <- variable
  commentAuthor <- variable
  issueComment  <- variable

  where_ $ do
    triplet dbTypeString dbTypeName "string"
    triplet dbTypeUint64 dbTypeName "uint64"
    triplet dbTypeRef    dbTypeName "ref"

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
    attribute userName      dbTypeString unique    single
    attribute issueTitle    dbTypeString unique    single
    attribute issueBody     dbTypeString nonUnique single
    attribute issueAuthor   dbTypeRef    nonUnique single
    attribute commentBody   dbTypeString nonUnique single
    attribute commentAuthor dbTypeRef    nonUnique single
    attribute issueComment  dbTypeRef    nonUnique many

  Schema
    -- TODO: Turen selected entity ids into attributes.
    -- Can it be done in a more type-safe manner?
    <$> select userName
    <*> select issueTitle
    <*> select issueBody
    <*> select issueAuthor
    <*> select commentBody
    <*> select commentAuthor
    <*> select issueComment
