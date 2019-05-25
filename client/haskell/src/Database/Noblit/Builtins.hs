{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Database.Noblit.Builtins
(
  attribute,
  attributeMany,
  attributeName,
  attributeUnique,
  typeName,
)
where

import Data.Text (Text)

import Database.Noblit.Primitive (EntityId, Value, encode)
import Database.Noblit.Query (Clause, triplet)
import Database.Noblit.Schema (Attribute (..), AttributeId, Datatype (..), typeEntityId)

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

attribute
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
attribute a name datatype unique many = do
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
