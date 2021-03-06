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

import Database.Noblit.Primitive (EntityId (..), Value, value)
import Database.Noblit.Query (Clause, Query, triplet, variable, where_)
import Database.Noblit.Schema (Attribute (..), AttributeId, Datatype (..), TypeId, typeEntityId)

import qualified Database.Noblit.Primitive as Primitive

-- NOTE: The magic entity ids here match those in Builtins defined in
-- database.rs.

-- db.type.name
typeName :: Attribute Text
typeName = AttrString (Primitive.value $ EntityId 5)

-- db.attribute.name
attributeName :: Attribute Text
attributeName = AttrString (Primitive.value $ EntityId 1)

-- db.attribute.type
attributeType :: Attribute EntityId
attributeType = AttrRef (Primitive.value $ EntityId 2)

-- db.attribute.unique
attributeUnique :: Attribute Bool
attributeUnique = AttrBool (Primitive.value $ EntityId 3)

-- db.attribute.many
attributeMany :: Attribute Bool
attributeMany = AttrBool (Primitive.value $ EntityId 4)

-- db.type.bool
isTypeBool :: Value vt TypeId => vt -> Clause (Datatype Bool)
isTypeBool t = do
  triplet t typeName ("db.type.bool" :: Text)
  pure $ TypeBool $ value t

-- db.type.ref
isTypeRef :: Value vt TypeId => vt -> Clause (Datatype EntityId)
isTypeRef t = do
  triplet t typeName ("db.type.ref" :: Text)
  pure $ TypeRef $ value t

-- db.type.uint64
isTypeUint64 :: Value vt TypeId => vt -> Clause (Datatype Word64)
isTypeUint64 t = do
  triplet t typeName ("db.type.uint64" :: Text)
  pure $ TypeUint64 $ value t

-- db.type.string
isTypeString :: Value vt TypeId => vt -> Clause (Datatype Text)
isTypeString t = do
  triplet t typeName ("db.type.string" :: Text)
  pure $ TypeString $ value t

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
    TypeBool _   -> AttrBool (value a)
    TypeRef _    -> AttrRef (value a)
    TypeUint64 _ -> AttrUint64 (value a)
    TypeBytes _  -> AttrBytes (value a)
    TypeString _ -> AttrString (value a)
