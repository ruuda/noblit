{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Database.Noblit.Schema
(
  Attribute (..),
  Datatype (..),
  TransactionId,
  AttributeId,
  TypeId,
  attributeEntityId,
  typeEntityId,
)
where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Data.Word (Word64)
import Database.Noblit.Primitive (EntityId, QueryValue)

type TransactionId = EntityId
type AttributeId = EntityId
type TypeId = EntityId

data Attribute :: * -> * where
  AttrBool   :: QueryValue EntityId -> Attribute Bool
  AttrRef    :: QueryValue EntityId -> Attribute EntityId
  AttrUint64 :: QueryValue EntityId -> Attribute Word64
  AttrBytes  :: QueryValue EntityId -> Attribute ByteString
  AttrString :: QueryValue EntityId -> Attribute Text

data Datatype :: * -> * where
  TypeBool   :: QueryValue EntityId -> Datatype Bool
  TypeRef    :: QueryValue EntityId -> Datatype EntityId
  TypeUint64 :: QueryValue EntityId -> Datatype Word64
  TypeBytes  :: QueryValue EntityId -> Datatype ByteString
  TypeString :: QueryValue EntityId -> Datatype Text

attributeEntityId :: Attribute a -> QueryValue EntityId
attributeEntityId a = case a of
  AttrBool aid   -> aid
  AttrRef aid    -> aid
  AttrUint64 aid -> aid
  AttrBytes aid  -> aid
  AttrString aid -> aid

typeEntityId :: Datatype a -> QueryValue EntityId
typeEntityId a = case a of
  TypeBool tyid   -> tyid
  TypeRef tyid    -> tyid
  TypeUint64 tyid -> tyid
  TypeBytes tyid  -> tyid
  TypeString tyid -> tyid

