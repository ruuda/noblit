{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Noblit.Builtins
(
  Datatypes (..),
  datatypes,
  attributeMany,
  attributeName,
  attributeUnique,
  typeName,
  isAttribute,
  isTypeBool,
  isTypeRef,
  isTypeString,
  isTypeUint64,
)
where

import Data.Text (Text)
import Data.Word (Word64)

import Database.Noblit.Primitive (EntityId, Value, encode)
import Database.Noblit.Query (Clause, Query, triplet, variable, where_)
import Database.Noblit.Schema (Attribute (..), AttributeId, Datatype (..), TypeId, typeEntityId)

-- db.type.name
typeName :: Attribute Text
typeName = undefined

-- db.attribute.name
attributeName :: Attribute Text
attributeName = undefined

-- db.attribute.type
attributeType :: Attribute EntityId
attributeType = undefined

-- db.attribute.unique
attributeUnique :: Attribute Bool
attributeUnique = undefined

-- db.attribute.many
attributeMany :: Attribute Bool
attributeMany = undefined

-- db.type.bool
isTypeBool :: Value vt TypeId => vt -> Clause (Datatype Bool)
isTypeBool t = do
  triplet t typeName ("db.type.bool" :: Text)
  pure $ TypeBool $ encode t

-- db.type.ref
isTypeRef :: Value vt TypeId => vt -> Clause (Datatype EntityId)
isTypeRef t = do
  triplet t typeName ("db.type.ref" :: Text)
  pure $ TypeRef $ encode t

-- db.type.uint64
isTypeUint64 :: Value vt TypeId => vt -> Clause (Datatype Word64)
isTypeUint64 t = do
  triplet t typeName ("db.type.uint64" :: Text)
  pure $ TypeUint64 $ encode t

-- db.type.string
isTypeString :: Value vt TypeId => vt -> Clause (Datatype Text)
isTypeString t = do
  triplet t typeName ("db.type.string" :: Text)
  pure $ TypeString $ encode t

data Datatypes = Datatypes
  { typeBool   :: Datatype Bool
  , typeRef    :: Datatype EntityId
  , typeUint64 :: Datatype Word64
  , typeString :: Datatype Text
  }

datatypes :: Query q => q Datatypes
datatypes = do
  tBool   <- variable
  tRef    <- variable
  tUint64 <- variable
  tString <- variable
  where_ $ Datatypes
    <$> isTypeBool tBool
    <*> isTypeRef tRef
    <*> isTypeUint64 tUint64
    <*> isTypeString tString

isAttribute
  :: Value va AttributeId
  => Value vn Text
  => Value vu Bool
  => Value vm Bool
  => va
  -> vn
  -> Datatype a
  -> vu
  -> vm
  -> Clause (Attribute a)
isAttribute a name datatype unique many = do
  triplet a attributeName name
  triplet a attributeType (typeEntityId datatype)
  triplet a attributeUnique unique
  triplet a attributeMany many
  pure $ case datatype of
    TypeBool _   -> AttrBool (encode a)
    TypeRef _    -> AttrRef (encode a)
    TypeUint64 _ -> AttrUint64 (encode a)
    TypeBytes _  -> AttrBytes (encode a)
    TypeString _ -> AttrString (encode a)
