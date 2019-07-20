{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Database.Noblit.Primitive
(
  Constant (..),
  EntityId (..),
  Blank (..),
  Value (..),
  Variable (..),
  VariableId (..),
  QueryValue (..),
)
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32, Word64)

newtype EntityId = EntityId Word64
newtype VariableId = VariableId Word32

data Variable :: * -> * where
  VariableBool     :: VariableId -> Variable Bool
  VariableRef      :: VariableId -> Variable EntityId
  VariableUint64   :: VariableId -> Variable Word64
  VariableBytes    :: VariableId -> Variable ByteString
  VariableString   :: VariableId -> Variable Text

data Constant :: * -> * where
  ConstantBool     :: Bool       -> Constant Bool
  ConstantRef      :: EntityId   -> Constant EntityId
  ConstantUint64   :: Word64     -> Constant Word64
  ConstantBytes    :: ByteString -> Constant ByteString
  ConstantString   :: Text       -> Constant Text

data QueryValue :: * -> * where
  ValueVariable :: Variable a -> QueryValue a
  ValueConstant :: Constant a -> QueryValue a

class Blank v where
  variable :: VariableId -> Variable v

class Value v a | v -> a where
  encode :: v -> QueryValue a

instance Blank Bool       where variable = VariableBool
instance Blank EntityId   where variable = VariableRef
instance Blank Word64     where variable = VariableUint64
instance Blank ByteString where variable = VariableBytes
instance Blank Text       where variable = VariableString

instance Value (QueryValue a) a          where encode = id
instance Value (Variable a)   a          where encode v = ValueVariable v
instance Value Bool           Bool       where encode x = ValueConstant $ ConstantBool x
instance Value EntityId       EntityId   where encode x = ValueConstant $ ConstantRef x
instance Value Word64         Word64     where encode x = ValueConstant $ ConstantUint64 x
instance Value ByteString     ByteString where encode x = ValueConstant $ ConstantBytes x
instance Value Text           Text       where encode x = ValueConstant $ ConstantString x
